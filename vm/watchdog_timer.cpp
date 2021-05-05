// vim: set ts=8 sts=2 sw=2 tw=99 et:
//
// This file is part of SourcePawn.
// 
// SourcePawn is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// SourcePawn is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with SourcePawn.  If not, see <http://www.gnu.org/licenses/>.
#include "watchdog_timer.h"

#include <string.h>

#ifdef __EMSCRIPTEN__
#ifndef pthread_setname_np
extern "C" int pthread_setname_np(pthread_t thread, const char *name)
{
  emscripten_set_thread_name(thread, name);
  return 0;
}
#endif
#endif

#include <amtl/am-thread.h>
#include "environment.h"

using namespace sp;

#ifdef __EMSCRIPTEN__
// When using emscripten's pthread stubs, prctl is missing and causes a link error.
extern "C" int __attribute__((weak)) prctl(int, ...)
{
  errno = EINVAL;
  return -1;
}
#endif

WatchdogTimer::WatchdogTimer(Environment* env)
 : env_(env),
   terminate_(false),
   mainthread_(std::this_thread::get_id()),
   ignore_timeout_(false),
   last_frame_id_(0),
   second_timeout_(false),
   timedout_(false)
{
}

WatchdogTimer::~WatchdogTimer()
{
  assert(!thread_);
}

bool
WatchdogTimer::Initialize(size_t timeout_ms)
{
  if (thread_)
    return false;

  timeout_ms_ = timeout_ms;

  std::lock_guard<std::mutex> lock(mutex_);
  thread_ = ke::NewThread("SourcePawn Watchdog", [this]() -> void {
    Run();
  });
  return true;
}

void
WatchdogTimer::Shutdown()
{
  if (terminate_ || !thread_)
    return;

  {
    std::lock_guard<std::mutex> lock(mutex_);
    terminate_ = true;
    cv_.notify_all();
  }
  thread_->join();
  thread_ = nullptr;
}

void
WatchdogTimer::Run()
{
  std::unique_lock<std::mutex> lock(mutex_);

  // Initialize the frame id, so we don't have to wait longer on startup.
  last_frame_id_ = env_->FrameId();

  while (!terminate_) {
    auto rv = cv_.wait_for(lock, std::chrono::milliseconds(timeout_ms_ / 2));
    if (terminate_)
      return;

    if (rv != std::cv_status::timeout)
      continue;

    // We reached a timeout. If the current frame is not equal to the last
    // frame, then we assume the server is still moving enough to process
    // frames. We also make sure JIT code is actually on the stack, since
    // our concept of frames won't move if JIT code is not running.
    // If we're told to ignore timeouts, we do so.
    //
    // Note that it's okay if these two race: it's just a heuristic, and
    // worst case, we'll miss something that might have timed out but
    // ended up resuming.
    uintptr_t frame_id = env_->FrameId();
    if (frame_id != last_frame_id_ || !env_->RunningCode() || ignore_timeout_) {
      last_frame_id_ = frame_id;
      second_timeout_ = false;
      continue;
    }

    // We woke up with the same frame. We might be timing out. Wait another
    // time to see if so.
    if (!second_timeout_) {
      second_timeout_ = true;
      continue;
    }

    {
      // Prevent the JIT from linking or destroying runtimes and functions.
      std::lock_guard<ke::Mutex> lock(env_->lock());

      // Set the timeout notification bit. If this is detected before any patched
      // JIT backedges are reached, the main thread will attempt to acquire the
      // monitor lock, and block until we call Wait().
      timedout_ = true;
    
      // Patch all jumps. This can race with the main thread's execution since
      // all code writes are 32-bit integer instruction operands, which are
      // guaranteed to be atomic on x86.
      env_->PatchAllJumpsForTimeout();
    }

    // The JIT will be free to compile new functions while we wait, but it will
    // see the timeout bit set above and immediately bail out.
    cv_.wait(lock);

    second_timeout_ = false;

    // Reset the last frame ID to something unlikely to be chosen again soon.
    last_frame_id_--;

    // It's possible that Shutdown() raced with the timeout, and if so, we
    // must leave now to prevent a deadlock.
    if (terminate_)
      return;
  }
}

bool
WatchdogTimer::NotifyTimeoutReceived()
{
  // We are guaranteed that the watchdog thread is waiting for our
  // notification, and is therefore blocked. We take the JIT lock
  // anyway for sanity.
  {
    std::lock_guard<ke::Mutex> lock(env_->lock());
    env_->UnpatchAllJumpsFromTimeout();
  }

  timedout_ = false;

  // Wake up the watchdog thread, it's okay to keep processing now.
  std::lock_guard<std::mutex> lock(mutex_);
  cv_.notify_all();
  return false;
}

bool
WatchdogTimer::HandleInterrupt()
{
  if (timedout_)
    return NotifyTimeoutReceived();
  return true;
}
