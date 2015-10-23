// vim: set sts=2 ts=8 sw=2 tw=99 et:
// 
// Copyright (C) 2006-2015 AlliedModders LLC
// 
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
// Based on pawndbg, the Pawn debugger, by ITB CompuPhase.
//
#include "console-debugger.h"
#include "smx-v1-image.h"
#include <cctype>
#include "sp_typeutil.h"
#include "plugin-context.h"
#include "environment.h"
#include "jit.h"
#include "watchdog_timer.h"

#if defined __GNUC__
#include <unistd.h>
#endif
#if defined __linux__
#include <termios.h>
#endif
#if defined WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

namespace sp {

enum {
  DISP_DEFAULT = 0x10,
  DISP_STRING = 0x20,
  DISP_BIN = 0x30,   /* ??? not implemented */
  DISP_HEX = 0x40,
  DISP_BOOL = 0x50,
  DISP_FIXED = 0x60,   /* unused in sourcepawn */
  DISP_FLOAT = 0x70,
};
#define DISP_MASK 0x0f

  // Convince debugging console to act more like an
  // interactive input.
#if defined __linux__
tcflag_t GetTerminalLocalMode()
{
  struct termios term;
  tcgetattr(STDIN_FILENO, &term);
  return term.c_lflag;
}

void SetTerminalLocalMode(tcflag_t flag)
{
  struct termios term;
  tcgetattr(STDIN_FILENO, &term);
  term.c_lflag = flag;
  tcsetattr(STDIN_FILENO, TCSANOW, &term);
}

tcflag_t EnableTerminalEcho()
{
  tcflag_t flags = GetTerminalLocalMode();
  tcflag_t old_flags = flags;
  flags |= (ICANON | ECHO | ECHOE | ECHOK | ECHOCTL | IEXTEN);
  SetTerminalLocalMode(flags);
  return old_flags;
}

void ResetTerminalEcho(tcflag_t flag)
{
  SetTerminalLocalMode(flag);
}
#endif

#if defined WIN32
DWORD EnableTerminalEcho()
{
  DWORD mode, old_mode;
  HANDLE hConsole = GetStdHandle(STD_INPUT_HANDLE);
  GetConsoleMode(hConsole, &mode);
  old_mode = mode;
  mode |= (ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT | ENABLE_EXTENDED_FLAGS | ENABLE_INSERT_MODE);
  SetConsoleMode(hConsole, mode);
  return old_mode;
}

void ResetTerminalEcho(DWORD mode)
{
  HANDLE hConsole = GetStdHandle(STD_INPUT_HANDLE);
  SetConsoleMode(hConsole, mode);
}
#endif

int InvokeDebugger(PluginContext *ctx)
{
  if (!ctx->IsDebugging())
    return SP_ERROR_NOTDEBUGGING;

  Debugger *debugger = ctx->GetDebugger();
  if (!debugger->active())
    return SP_ERROR_NONE;

  FrameIterator iter;
  cell_t cip = 0;
  // Find first scripted frame on the stack to get the cip from.
  while (!iter.Done()) {
    if (iter.IsScriptedFrame()) {
      cip = iter.cip();
      break;
    }
    iter.Next();
  }

  debugger->SetBreakCount(debugger->breakcount() + 1);
  Runmode orig_runmode = debugger->runmode();

  // when running until the function exit, check the frame address
  if (debugger->runmode() == Runmode::STEPOUT &&
    ctx->frm() > debugger->lastframe())
  {
    debugger->SetRunmode(Runmode::STEPPING);
  }

  bool isBreakpoint = false;
  // when running, check the breakpoints
  if (debugger->runmode() != Runmode::STEPPING &&
    debugger->runmode() != Runmode::STEPOVER)
  {
    // check breakpoint address
    isBreakpoint = debugger->CheckBreakpoint(cip);
    if (!isBreakpoint)
      return SP_ERROR_NONE;

    debugger->SetRunmode(Runmode::STEPPING);
  }

  // try to avoid halting on the same line twice
  uint32_t line = 0;
  if (ctx->runtime()->LookupLine(cip, &line) == SP_ERROR_NONE) {
    // assume that there are no more than 5 breaks on a single line.
    // if there are, halt.
    if (line == debugger->lastline() && debugger->breakcount() < 5) {
      debugger->SetRunmode(orig_runmode);
      return SP_ERROR_NONE;
    }
  }
  debugger->SetLastLine(line);
  debugger->SetBreakCount(0);

  // check whether we are stepping through a sub-function
  if (debugger->runmode() == Runmode::STEPOVER) {
    assert(debugger->lastframe() != 0);
    if (ctx->frm() < debugger->lastframe())
      return SP_ERROR_NONE;
  }

  const char *filename;
  if (ctx->runtime()->LookupFile(cip, &filename) == SP_ERROR_NONE) {
    debugger->SetCurrentFile(filename);
  }

  // Tell the watchdog to take a break.
  Environment::get()->watchdog()->SetIgnore(true);

  // Echo input back and enable basic control
  unsigned int old_flags = EnableTerminalEcho();
  debugger->HandleInput(cip, isBreakpoint);
  ResetTerminalEcho(old_flags);

  // Enable the watchdog timer again.
  Environment::get()->watchdog()->SetIgnore(false);

  // step OVER functions (so save the stack frame)
  if (debugger->runmode() == Runmode::STEPOVER ||
    debugger->runmode() == Runmode::STEPOUT)
    debugger->SetLastFrame(ctx->frm());

  return SP_ERROR_NONE;
}

Debugger::Debugger(PluginContext *context)
  : context_(context),
  runmode_(RUNNING),
  lastfrm_(0),
  lastline_(-1),
  currentfile_(""),
  active_(false)
{
}

bool
Debugger::Initialize()
{
  if (!breakpoint_map_.init())
    return false;

  if (!watch_table_.init())
    return false;

  return true;
}

bool
Debugger::active()
{
  return active_;
}

void
Debugger::Activate()
{
  // TODO: Patch all breaks to call the debug handler
  active_ = true;
}

void
Debugger::Deactivate()
{
  // TODO: Unpatch calls
  active_ = false;

  ClearAllBreakpoints();
  ClearAllWatches();
  SetRunmode(RUNNING);
}

void
Debugger::ReportError(const IErrorReport& report, FrameIterator& iter)
{
  if (report.IsFatal())
    printf("STOP on FATAL exception: %s\n", report.Message());
  else
    printf("STOP on exception: %s\n", report.Message());

  iter.Reset();

  // Find the nearest scripted frame.
  for (; !iter.Done(); iter.Next()) {
    if (iter.IsScriptedFrame()) {
      break;
    }
  }

  cell_t cip = iter.cip();

  uint32_t line = 0;
  context_->runtime()->LookupLine(cip, &line);
  SetLastLine(line);

  const char *filename;
  if (context_->runtime()->LookupFile(cip, &filename) == SP_ERROR_NONE) {
    SetCurrentFile(filename);
  }

  // Tell the watchdog to take a break.
  Environment::get()->watchdog()->SetIgnore(true);

  // Echo input back and enable basic control
  unsigned int old_flags = EnableTerminalEcho();
  HandleInput(cip, false);
  ResetTerminalEcho(old_flags);

  // Enable the watchdog timer again.
  Environment::get()->watchdog()->SetIgnore(false);

  // step OVER functions (so save the stack frame)
  if (runmode() == Runmode::STEPOVER ||
    runmode() == Runmode::STEPOUT)
    SetLastFrame(context_->frm());
}

Runmode
Debugger::runmode()
{
  return runmode_;
}

void
Debugger::SetRunmode(Runmode runmode)
{
  runmode_ = runmode;
}

cell_t
Debugger::lastframe()
{
  return lastfrm_;
}

void
Debugger::SetLastFrame(cell_t lastfrm)
{
  lastfrm_ = lastfrm;
}

uint32_t
Debugger::lastline()
{
  return lastline_;
}

void
Debugger::SetLastLine(uint32_t line)
{
  lastline_ = line;
}

uint32_t
Debugger::breakcount()
{
  return breakcount_;
}

void
Debugger::SetBreakCount(uint32_t breakcount)
{
  breakcount_ = breakcount;
}

const char *
Debugger::currentfile()
{
  return currentfile_;
}

void
Debugger::SetCurrentFile(const char *file)
{
  currentfile_ = file;
}

static char *strstrip(char *string)
{
  int pos;

  /* strip leading white space */
  while (*string != '\0' && *string <= ' ')
    memmove(string, string + 1, strlen(string));
  /* strip trailing white space */
  for (pos = strlen(string); pos>0 && string[pos - 1] <= ' '; pos--)
    string[pos - 1] = '\0';
  return string;
}

static char *skipwhitespace(char *str)
{
  while (*str == ' ' || *str == '\t')
    str++;
  return (char*)str;
}

static const char *skippath(const char *str)
{
  const char *p1, *p2;

  /* DOS/Windows pathnames */
  if ((p1 = strrchr(str, '\\')) != NULL)
    p1++;
  else
    p1 = str;
  if (p1 == str && p1[1] == ':')
    p1 = str + 2;
  /* Unix pathnames */
  if ((p2 = strrchr(str, '/')) != NULL)
    p2++;
  else
    p2 = str;
  return p1>p2 ? p1 : p2;
}

void
Debugger::ListCommands(char *command)
{
  if (command == nullptr)
    command = (char *)"";

  if (strlen(command) == 0 || !strcmp(command, "?")) {
    printf("At the prompt, you can type debug commands. For example, the word \"step\" is a\n"
      "command to execute a single line in the source code. The commands that you will\n"
      "use most frequently may be abbreviated to a single letter: instead of the full\n"
      "word \"step\", you can also type the letter \"s\" followed by the enter key.\n\n"
      "Available commands:\n");
  }
  else {
    printf("Options for command \"%s\":\n", command);
  }

  if (!stricmp(command, "break") || !stricmp(command, "tbreak")) {
    printf("\tUse TBREAK for one-time breakpoints\n\n"
      "\tBREAK\t\tlist all breakpoints\n"
      "\tBREAK n\t\tset a breakpoint at line \"n\"\n"
      "\tBREAK name:n\tset a breakpoint in file \"name\" at line \"n\"\n"
      "\tBREAK func\tset a breakpoint at function with name \"func\"\n"
      "\tBREAK .\t\tset a breakpoint at the current location\n");
  }
  else if (!stricmp(command, "cbreak")) {
    printf("\tCBREAK n\tremove breakpoint number \"n\"\n"
      "\tCBREAK *\tremove all breakpoints\n");
  }
  else if (!stricmp(command, "cw") || !stricmp(command, "cwatch")) {
    printf("\tCWATCH may be abbreviated to CW\n\n"
      "\tCWATCH n\tremove watch number \"n\"\n"
      "\tCWATCH var\tremove watch from \"var\"\n"
      "\tCWATCH *\tremove all watches\n");
  }
  else if (!stricmp(command, "d") || !stricmp(command, "disp")) {
    printf("\tDISP may be abbreviated to D\n\n"
      "\tDISP\t\tdisplay all variables that are currently in scope\n"
      "\tDISP var\tdisplay the value of variable \"var\"\n"
      "\tDISP var[i]\tdisplay the value of array element \"var[i]\"\n");
  }
  else if (!stricmp(command, "f") || !stricmp(command, "frame")) {
    printf("\tFRAME may be abbreviated to F\n\n");
    printf("\tFRAME n\tselect frame n and show/change local variables in that function\n");
  }
  else if (!stricmp(command, "g") || !stricmp(command, "go")) {
    printf("\tGO may be abbreviated to G\n\n"
      "\tGO\t\trun until the next breakpoint or program termination\n"
      "\tGO n\t\trun until line number \"n\"\n"
      "\tGO name:n\trun until line number \"n\" in file \"name\"\n"
      "\tGO func\t\trun until the current function returns (\"step out\")\n");
  }
  else if (!stricmp(command, "set")) {
    printf("\tSET var=value\t\tset variable \"var\" to the numeric value \"value\"\n"
      "\tSET var[i]=value\tset array item \"var\" to a numeric value\n"
      "\tSET var=\"value\"\t\tset string variable \"var\" to string \"value\"\n");
  }
  else if (!stricmp(command, "type")) {
    printf("\tTYPE var STRING\t\tdisplay \"var\" as string\n"
      "\tTYPE var STD\t\tset default display format (decimal integer)\n"
      "\tTYPE var HEX\t\tset hexadecimal integer format\n"
      "\tTYPE var FLOAT\t\tset floating point format\n");
  }
  else if (!stricmp(command, "watch") || !stricmp(command, "w")) {
    printf("\tWATCH may be abbreviated to W\n\n"
      "\tWATCH var\tset a new watch at variable \"var\"\n");
  }
  else if (!stricmp(command, "x")) {
    printf("\tX/FMT ADDRESS\texamine plugin memory at \"ADDRESS\"\n"
      "\tADDRESS is an expression for the memory address to examine.\n"
      "\tFMT is a repeat count followed by a format letter and a size letter.\n"
      "\t\tFormat letters are o(octal), x(hex), d(decimal), u(unsigned decimal),\n"
      "\t\t\tf(float), c(char) and s(string).\n"
      "\t\tSize letters are b(byte), h(halfword), w(word).\n\n"
      "\t\tThe specified number of objects of the specified size are printed\n"
      "\t\taccording to the format.\n\n"
      //"\tDefaults for format and size letters are those previously used.\n"
      //"\tDefault count is 1.  Default address is following last thing printed\n"
      //"\twith this command or \"disp\".\n"
      );
  }
  else if (!stricmp(command, "n") || !stricmp(command, "next") ||
    !stricmp(command, "quit") || !stricmp(command, "pos") ||
    !stricmp(command, "s") || !stricmp(command, "step") ||
    !stricmp(command, "files"))
  {
    printf("\tno additional information\n");
  }
  else {
    printf("\tB(ack)T(race)\t\tdisplay the stack trace\n"
      "\tBREAK\t\tset breakpoint at line number or variable name\n"
      "\tCBREAK\t\tremove breakpoint\n"
      "\tCW(atch)\tremove a \"watchpoint\"\n"
      "\tD(isp)\t\tdisplay the value of a variable, list variables\n"
      "\tFILES\t\tlist all files that this program is composed off\n"
      "\tF(rame)\t\tSelect a frame from the back trace to operate on\n"
      "\tFUNCS\t\tdisplay functions\n"
      "\tG(o)\t\trun program (until breakpoint)\n"
      "\tN(ext)\t\tRun until next line, step over functions\n"
      "\tPOS\t\tShow current file and line\n"
      "\tQUIT\t\texit debugger\n"
      "\tSET\t\tSet a variable to a value\n"
      "\tS(tep)\t\tsingle step, step into functions\n"
      "\tTYPE\t\tset the \"display type\" of a symbol\n"
      "\tW(atch)\t\tset a \"watchpoint\" on a variable\n"
      "\tX\t\texamine plugin memory: x/FMT ADDRESS\n"
      "\n\tUse \"? <command name>\" to view more information on a command\n");
  }
}

void
Debugger::HandleInput(cell_t cip, bool isBp)
{
  static char lastcommand[32] = "";
  LegacyImage *image = context_->runtime()->image();

  FrameIterator frames;
  frames_ = &frames;
  frame_count_ = 0;
  selected_frame_ = 0;
  cip_ = cip;
  frm_ = context_->frm();
  selected_context_ = context_;

  // Count the frames
  // Select first scripted frame, if it's not frame 0
  bool selected_first_scripted = false;
  for (; !frames.Done(); frames.Next(), frame_count_++) {
    if (!selected_first_scripted && frames.IsScriptedFrame()) {
      selected_frame_ = frame_count_;
      selected_first_scripted = true;
    }
  }

  frames.Reset();

  if (!isBp)
    printf("STOP at line %d in %s\n", lastline_, skippath(currentfile_));
  else
    printf("BREAK at line %d in %s\n", lastline_, skippath(currentfile_));

  // Print all watched variables now.
  ListWatches();

  char line[512], command[32];
  int result;
  char *params;
  for (;;) {
    fgets(line, sizeof(line), stdin);

    // strip newline character, plus leading or trailing white space
    strstrip(line);

    // Repeat the last command, if no new command was given.
    if (strlen(line) == 0) {
      strncpy(line, lastcommand, sizeof(line));
    }
    lastcommand[0] = '\0';

    result = sscanf(line, "%8s", command);
    if (result <= 0) {
      ListCommands(nullptr);
      continue;
    }

    params = strchr(line, ' ');
    params = (params != nullptr) ? skipwhitespace(params) : (char*)"";

    if (!stricmp(command, "?")) {
      result = sscanf(line, "%*s %30s", command);
      ListCommands(result ? command : nullptr);
    }
    else if (!stricmp(command, "quit")) {
      fputs("Clearing all breakpoints. Running normally.\n", stdout);
      Deactivate();
      return;
    }
    else if (!stricmp(command, "g") || !stricmp(command, "go")) {
      if (!stricmp(params, "func")) {
        SetRunmode(STEPOUT);
        return;
      }

      // Run until that line
      if (*params != '\0') {
        const char *filename = currentfile_;
        params = ParseBreakpointLine(params, &filename);
        if (params == nullptr)
          continue;

        Breakpoint *bp = nullptr;
        // User specified a line number
        if (isdigit(*params)) {
          bp = AddBreakpoint(filename, strtol(params, NULL, 10) - 1, true);
        }

        if (bp == nullptr) {
          fputs("Invalid format or bad breakpoint address. Type ? go for the syntax.\n", stdout);
          return;
        }
        
        uint32_t bpline = 0;
        image->LookupLine(bp->addr(), &bpline);
        printf("Running until line %d in file %s.\n", bpline, skippath(filename));
      }

      SetRunmode(RUNNING);
      return;
    }
    else if (!stricmp(command, "s") || !stricmp(command, "step")) {
      strncpy(lastcommand, "s", sizeof(lastcommand));
      SetRunmode(STEPPING);
      return;
    }
    else if (!stricmp(command, "n") || !stricmp(command, "next")) {
      strncpy(lastcommand, "n", sizeof(lastcommand));
      SetRunmode(STEPOVER);
      return;
    }
    else if (!stricmp(command, "funcs")) {
      fputs("Listing functions:\n", stdout);
      SmxV1Image *imagev1 = (SmxV1Image *)image;
      SmxV1Image::SymbolIterator iter = imagev1->symboliterator();
      while (!iter.Done()) {
        const SmxV1Image::Symbol sym = iter.Next();
        if (sym.ident() == sp::IDENT_FUNCTION &&
          imagev1->GetDebugName(sym.name()) != nullptr)
        {
          printf("%s", imagev1->GetDebugName(sym.name()));
          const char *filename = image->LookupFile(sym.addr());
          if (filename != nullptr) {
            printf("\t(%s)", skippath(filename));
          }
          fputs("\n", stdout);
        }
      }
    }
    else if (!stricmp(command, "bt") || !stricmp(command, "backtrace")) {
      printf("Stack trace:\n");
      DumpStack();
    }
    else if (!stricmp(command, "f") || !stricmp(command, "frame")) {
      if (*params == '\0' || !isdigit(*params)) {
        fputs("Invalid syntax. Type \"? frame\" for help.\n", stdout);
        continue;
      }

      uint32_t frame = atoi(params);
      if (frame < 0 || frame_count_ <= frame) {
        printf("Invalid frame. There are only %d frames on the stack.\n", frame_count_);
        continue;
      }

      if (frame == selected_frame_) {
        fputs("This frame is already selected.\n", stdout);
        continue;
      }

      // Select this frame to operate on.
      frames_->Reset();
      uint32_t index = 0;
      for (; !frames_->Done(); frames_->Next(), index++) {
        // Iterator is at the chosen frame now.
        if (index == frame)
          break;
      }

      if (!frames_->IsScriptedFrame()) {
        printf("%d is not a scripted frame.\n", frame);
        continue;
      }

      // Get the plugin context of the target frame
      selected_context_ = (PluginContext *)frames_->Context();
      image = selected_context_->runtime()->image();

      // Reset the frame iterator again and count all above frames in the context.
      frames_->Reset();
      index = 0;
      uint32_t num_scripted_frames = 0;
      for (; !frames_->Done(); frames_->Next(), index++) {
        // Count the scripted frames in the context to find the right frm pointer.
        if (frames_->IsScriptedFrame() && frames_->Context() == selected_context_)
          num_scripted_frames++;
        // We've reached the chosen frame.
        if (index == frame)
          break;
      }

      // Update internal state for this frame.
      selected_frame_ = frame;
      cip_ = frames_->cip();
      currentfile_ = image->LookupFile(cip_);
      image->LookupLine(cip_, &lastline_);

      // Find correct new frame pointer.
      cell_t frm = selected_context_->frm();
      for (uint32_t i = 1; i < num_scripted_frames; i++) {
        frm = *(cell_t *)(selected_context_->memory() + frm + 4);
      }
      frm_ = frm;

      printf("Selected frame %d.\n", frame);
    }
    else if (!stricmp(command, "break") || !stricmp(command, "tbreak")) {
      if (*params == '\0') {
        ListBreakpoints();
      }
      else {
        const char *filename = currentfile_;
        params = ParseBreakpointLine(params, &filename);
        if (params == nullptr)
          continue;

        Breakpoint *bp;
        // User specified a line number
        if (isdigit(*params)) {
          bp = AddBreakpoint(filename, strtol(params, NULL, 10) - 1, !stricmp(command, "tbreak"));
        }
        // User wants to add a breakpoint at the current location
        else if (*params == '.') {
          bp = AddBreakpoint(filename, lastline_ - 1, !stricmp(command, "tbreak"));
        }
        // User specified a function name
        else {
          bp = AddBreakpoint(filename, params, !stricmp(command, "tbreak"));
        }

        if (bp == nullptr) {
          fputs("Invalid breakpoint\n", stdout);
        }
        else {
          uint32_t bpline = 0;
          image->LookupLine(bp->addr(), &bpline);
          printf("Set breakpoint %d in file %s on line %d", breakpoint_map_.elements(), skippath(filename), bpline);
          if (bp->name() != nullptr)
            printf(" in function %s", bp->name());
          fputs("\n", stdout);
        }
      }
    }
    else if (!stricmp(command, "cbreak")) {
      if (*params == '*') {
        // clear all breakpoints
        ClearAllBreakpoints();
      }
      else {
        int number = FindBreakpoint(params);
        if (number < 0 || !ClearBreakpoint(number))
          fputs("\tUnknown breakpoint (or wrong syntax)\n", stdout);
        else
          printf("\tCleared breakpoint %d\n", number);
      }
    }
    else if (!stricmp(command, "disp") || !stricmp(command, "d")) {
      uint32_t idx[sDIMEN_MAX];
      int dim = 0;
      memset(idx, 0, sizeof(idx));
      SmxV1Image *imagev1 = (SmxV1Image *)image;
      if (*params == '\0') {
        // display all variables that are in scope
        SmxV1Image::SymbolIterator iter = imagev1->symboliterator();
        while (!iter.Done()) {
          ke::AutoPtr<SmxV1Image::Symbol> sym;
          sym = iter.Next();
          if (sym->ident() != sp::IDENT_FUNCTION &&
            sym->codestart() <= (uint32_t)cip_ &&
            sym->codeend() >= (uint32_t)cip_)
          {
            printf("%s\t<%#8x>\t", (sym->vclass() & DISP_MASK) > 0 ? "loc" : "glb", (sym->vclass() & DISP_MASK) > 0 ? frm_ + sym->addr() : sym->addr());
            if (imagev1->GetDebugName(sym->name()) != nullptr) {
              printf("%s\t", imagev1->GetDebugName(sym->name()));
            }

            DisplayVariable(sym, idx, 0);
            fputs("\n", stdout);
          }
        }
      }
      // Display single variable
      else {
        ke::AutoPtr<SmxV1Image::Symbol> sym;
        char *indexptr = strchr(params, '[');
        char *behindname = nullptr;
        assert(dim == 0);
        // Parse all [x][y] dimensions
        while (indexptr != nullptr && dim < sDIMEN_MAX) {
          if (behindname == nullptr)
            behindname = indexptr;

          indexptr++;
          idx[dim++] = atoi(indexptr);
          indexptr = strchr(indexptr, '[');
        }

        // End the string before the first [ temporarily, 
        // so GetVariable only looks for the variable name.
        if (behindname != nullptr)
          *behindname = '\0';

        // find the symbol with the smallest scope
        if (imagev1->GetVariable(params, cip_, sym)) {
          // Add the [ back again
          if (behindname != nullptr)
            *behindname = '[';

          printf("%s\t<%#8x>\t%s\t", (sym->vclass() & DISP_MASK) > 0 ? "loc" : "glb", (sym->vclass() & DISP_MASK) > 0 ? frm_ + sym->addr() : sym->addr(), params);
          DisplayVariable(sym, idx, dim);
          fputs("\n", stdout);
        }
        else {
          fputs("\tSymbol not found, or not a variable\n", stdout);
        }
      }
    }
    else if (!stricmp(command, "set")) {
      char varname[32];
      char strvalue[1024];
      uint32_t index;
      cell_t value;
      ke::AutoPtr<SmxV1Image::Symbol> sym;
      strvalue[0] = '\0';
      if (sscanf(params, " %[^[ ][%d] = %d", varname, &index, &value) != 3) {
        index = 0;
        if (sscanf(params, " %[^= ] = %d", varname, &value) != 2) {
          varname[0] = '\0';
          if (sscanf(params, " %[^= ] = \"%[^\"]\"", varname, strvalue) != 2) {
            strvalue[0] = '\0';
          }
        }
      }

      if (varname[0] != '\0') {
        // find the symbol with the given range with the smallest scope
        SmxV1Image *imagev1 = (SmxV1Image *)image;
        if (imagev1->GetVariable(varname, cip_, sym)) {
          // user gave an integer as value
          if (strvalue[0] == '\0') {
            SetSymbolValue(sym, index, value);
            if (index > 0)
              printf("%s[%d] set to %d\n", varname, index, value);
            else
              printf("%s set to %d\n", varname, value);
          }
          // we have a string as value
          else {
            if ((sym->ident() != sp::IDENT_ARRAY
              && sym->ident() != sp::IDENT_REFARRAY)
              || sym->dimcount() != 1) {
              printf("%s is not a string.\n", varname);
            }
            else {
              SetSymbolString(sym, strvalue);
              printf("%s set to \"%s\"\n", varname, strvalue);
            }
          }
        }
        else {
          fputs("Symbol not found or not a variable\n", stdout);
        }
      }
      else {
        fputs("Invalid syntax for \"set\". Type \"? set\".\n", stdout);
      }
    }
    else if (!stricmp(command, "files")) {
      fputs("Source files:\n", stdout);
      // browse through the file table
      SmxV1Image *imagev1 = (SmxV1Image *)image;
      for (unsigned int i = 0; i < imagev1->GetFileCount(); i++) {
        if (imagev1->GetFileName(i) != nullptr) {
          printf("%s\n", imagev1->GetFileName(i));
        }
      }
    }
    // Change display format of symbol
    else if (!stricmp(command, "type")) {
      char symname[32], *ptr;
      for (ptr = params; *ptr != '\0' && *ptr != ' ' && *ptr != '\t'; ptr++)
        /* nothing */;
      int len = (int)(ptr - params);
      if (len == 0 || len > 31) {
        fputs("\tInvalid (or missing) symbol name\n", stdout);
      }
      else {
        ke::AutoPtr<SmxV1Image::Symbol> sym;
        strncpy(symname, params, len);
        symname[len] = '\0';
        params = skipwhitespace(ptr);

        SmxV1Image *imagev1 = (SmxV1Image *)image;
        if (imagev1->GetVariable(symname, cip_, sym)) {
          assert(sym != nullptr);
          if (!stricmp(params, "std")) {
            sym->setVClass((sym->vclass() & DISP_MASK) | DISP_DEFAULT);
          }
          else if (!stricmp(params, "string")) {
            // check array with single dimension
            if (!(sym->ident() == sp::IDENT_ARRAY || sym->ident() == sp::IDENT_REFARRAY) ||
              sym->dimcount() != 1)
              fputs("\t\"string\" display type is only valid for arrays with one dimension\n", stdout);
            else
              sym->setVClass((sym->vclass() & DISP_MASK) | DISP_STRING);
          }
          else if (!stricmp(params, "bin")) {
            sym->setVClass((sym->vclass() & DISP_MASK) | DISP_BIN);
          }
          else if (!stricmp(params, "hex")) {
            sym->setVClass((sym->vclass() & DISP_MASK) | DISP_HEX);
          }
          else if (!stricmp(params, "float")) {
            sym->setVClass((sym->vclass() & DISP_MASK) | DISP_FLOAT);
          }
          else {
            fputs("\tUnknown (or missing) display type\n", stdout);
          }
          ListWatches();
        }
        else {
          printf("\tUnknown symbol \"%s\"\n", symname);
        }
      }
    }
    else if (!stricmp(command, "pos")) {
      printf("\tfile: %s", skippath(currentfile_));

      const char *function = image->LookupFunction(cip_);
      if (function != nullptr)
        printf("\tfunction: %s", function);

      printf("\tline: %d", lastline_);

      if (selected_frame_ > 0)
        printf("\tframe: %d", selected_frame_);

      fputs("\n", stdout);
    }
    else if (!stricmp(command, "w") || !stricmp(command, "watch")) {
      if (strlen(params) == 0) {
        fputs("Missing variable name\n", stdout);
        continue;
      }
      if (AddWatch(params))
        ListWatches();
      else
        fputs("Invalid watch\n", stdout);
    }
    else if (!stricmp(command, "cw") || !stricmp(command, "cwatch")) {
      if (strlen(params) == 0) {
        fputs("Missing variable name\n", stdout);
        continue;
      }

      if (*params == '*') {
        ClearAllWatches();
      }
      else if (isdigit(*params)) {
        // Delete watch by index
        if (!ClearWatch(atoi(params)))
          fputs("Bad watch number\n", stdout);
      }
      else {
        if (!ClearWatch(params))
          fputs("Variable not watched\n", stdout);
      }
      ListWatches();
    }
    else if (command[0] == 'x' || command[0] == 'X') {
      char* fmt = command + 1;

      if (strlen(params) == 0) {
        fputs("Missing address.\n", stdout);
        continue;
      }

      // Format is x/[count][format][size] <address>
      if (*fmt != '/') {
        fputs("Bad format specifier.\n", stdout);
        continue;
      }
      fmt++;

      char* count_str = fmt;
      // Skip count number
      while (isdigit(*fmt)) {
        fmt++;
      }

      // Default count is 1.
      int count = 1;
      // Parse [count] number. The amount of stuff to display.
      if (count_str != fmt) {
        count = atoi(count_str);
        if (count <= 0) {
          fputs("Invalid count.\n", stdout);
          continue;
        }
      }

      // Format letters are o(octal), x(hex), d(decimal), u(unsigned decimal)
      // t(binary), f(float), a(address), i(instruction), c(char) and s(string).
      char* format = fmt++;
      if (*format != 'o' && *format != 'x' && *format != 'd' &&
        *format != 'u' && *format != 'f' && *format != 'c' &&
        *format != 's') {
        printf("Invalid format letter '%c'.\n", *format);
        continue;
      }

      // Size letters are b(byte), h(halfword), w(word).
      char* size_ltr = fmt;

      unsigned int size;
      unsigned int line_break;
      unsigned int mask;
      switch (*size_ltr) {
      case 'b':
        size = 1;
        line_break = 8;
        mask = 0x000000ff;
        break;
      case 'h':
        size = 2;
        line_break = 8;
        mask = 0x0000ffff;
        break;
      case 'w':
        // Default size is a word, if none was given.
      case '\0':
        size = 4;
        line_break = 4;
        mask = 0xffffffff;
        break;
      default:
        printf("Invalid size letter '%c'.\n", *size_ltr);
        continue;
      }

      // Skip the size letter.
      if (*size_ltr != '\0')
        fmt++;

      if (*fmt) {
        fputs("Invalid output format string.\n", stdout);
        continue;
      }

      // Parse address.
      // We support 4 "magic" addresses:
      // $cip: instruction pointer
      // $sp: stack pointer
      // $hp: heap pointer
      // $frm: frame pointer
      cell_t address = 0;
      if (*params == '$') {
        if (!stricmp(params, "$cip")) {
          address = cip_;
        }
        // TODO: adjust for selected frame like frm_.
        else if (!stricmp(params, "$sp")) {
          address = selected_context_->sp();
        }
        else if (!stricmp(params, "$hp")) {
          address = selected_context_->hp();
        }
        else if (!stricmp(params, "$frm")) {
          address = frm_;
        }
        else {
          printf("Unknown address %s.\n", params);
          continue;
        }
      }
      // This is a raw address.
      else {
        address = (cell_t)strtol(params, NULL, 0);
      }

      if (((address >= selected_context_->hp()) && (address < selected_context_->sp())) ||
        (address < 0) || ((ucell_t)address >= selected_context_->HeapSize())) {
        fputs("Address out of plugin's bounds.\n", stdout);
        continue;
      }

      // Print the memory
      // Create a format string for the desired output format.
      char fmt_string[16];
      switch (*format) {
      case 'd':
      case 'u':
        snprintf(fmt_string, sizeof(fmt_string), "%%%d%c", size * 2, *format);
        break;
      case 'o':
        snprintf(fmt_string, sizeof(fmt_string), "0%%0%d%c", size * 2, *format);
        break;
      case 'x':
        snprintf(fmt_string, sizeof(fmt_string), "0x%%0%d%c", size * 2, *format);
        break;
      case 's':
        strncpy(fmt_string, "\"%s\"", sizeof(fmt_string));
        break;
      case 'c':
        strncpy(fmt_string, "'%c'", sizeof(fmt_string));
        break;
      case 'f':
        strncpy(fmt_string, "%.2f", sizeof(fmt_string));
        break;
      default:
        continue;
      }

      cell_t *data;
      for (int i = 0; i<count; i++) {

        if (((address >= selected_context_->hp()) && (address < selected_context_->sp())) ||
          (address < 0) || ((ucell_t)address >= selected_context_->HeapSize()))
          break;

        if (i % line_break == 0) {
          if (i > 0)
            fputs("\n", stdout);
          printf("0x%x: ", address);
        }

        data = (cell_t *)(selected_context_->memory() + address);

        switch (*format) {
        case 'f':
          printf(fmt_string, sp_ctof(*data));
          break;
        case 'd':
        case 'u':
        case 'o':
        case 'x':
          printf(fmt_string, *data & mask);
          break;
        case 's':
          printf(fmt_string, (char*)data);
          break;
        default:
          printf(fmt_string, *data);
          break;
        }

        fputs("  ", stdout);

        // Move to the next address based on the size;
        address += size;
      }

      fputs("\n", stdout);
    }
    else {
      printf("\tInvalid command \"%s\", use \"?\" to view all commands\n", command);
    }
  }
}

bool
Debugger::CheckBreakpoint(cell_t cip)
{
  BreakpointMap::Result result = breakpoint_map_.find(cip);
  if (!result.found())
    return false;

  // Remove the temporary breakpoint
  if (result->value->temporary()) {
    ClearBreakpoint(result->value);
  }

  return true;
}

Breakpoint *
Debugger::AddBreakpoint(const char* file, uint32_t line, bool temporary)
{
  LegacyImage *image = context_->runtime()->image();
  const char *targetfile = image->FindFileByPartialName(file);
  if (targetfile == nullptr)
    targetfile = currentfile_;

  uint32_t addr;
  if (!image->GetLineAddress(line, targetfile, &addr))
    return nullptr;

  Breakpoint *bp;
  {
    BreakpointMap::Insert p = breakpoint_map_.findForAdd(addr);
    if (p.found())
      return p->value;

    bp = new Breakpoint(image, addr, nullptr, temporary);
    breakpoint_map_.add(p, addr, bp);
  }

  return bp;
}

Breakpoint *
Debugger::AddBreakpoint(const char* file, const char *function, bool temporary)
{
  LegacyImage *image = context_->runtime()->image();
  const char *targetfile = image->FindFileByPartialName(file);
  if (targetfile == nullptr)
    return nullptr;

  uint32_t addr;
  if (!image->GetFunctionAddress(function, targetfile, &addr))
    return nullptr;

  Breakpoint *bp;
  {
    BreakpointMap::Insert p = breakpoint_map_.findForAdd(addr);
    if (p.found())
      return p->value;

    const char *realname = image->LookupFunction(addr);

    bp = new Breakpoint(image, addr, realname, temporary);
    breakpoint_map_.add(p, addr, bp);
  }

  return bp;
}

bool
Debugger::ClearBreakpoint(int number)
{
  if (number <= 0)
    return false;

  int i = 0;
  for (BreakpointMap::iterator iter = breakpoint_map_.iter(); !iter.empty(); iter.next()) {
    if (++i == number) {
      iter.erase();
      return true;
    }
  }
  return false;
}

bool
Debugger::ClearBreakpoint(Breakpoint * bp)
{
  BreakpointMap::Result res = breakpoint_map_.find(bp->addr());
  if (!res.found())
    return false;

  breakpoint_map_.remove(res);
  return true;
}

void
Debugger::ClearAllBreakpoints()
{
  breakpoint_map_.clear();
}

int
Debugger::FindBreakpoint(char *breakpoint)
{
  LegacyImage *image = context_->runtime()->image();
  breakpoint = skipwhitespace(breakpoint);

  // check if a filename precedes the breakpoint location
  char *sep = strrchr(breakpoint, ':');
  if (sep == nullptr && isdigit(*breakpoint))
    return atoi(breakpoint);

  const char *filename = currentfile_;
  if (sep != nullptr) {
    /* the user may have given a partial filename (e.g. without a path), so
    * walk through all files to find a match
    */
    *sep = '\0';
    filename = image->FindFileByPartialName(breakpoint);
    if (filename == nullptr)
      return -1;

    // Skip past colon
    breakpoint = sep + 1;
  }

  breakpoint = skipwhitespace(breakpoint);
  Breakpoint *bp;
  const char *fname;
  uint32_t line;
  uint32_t number = 0;
  for (BreakpointMap::iterator iter = breakpoint_map_.iter(); !iter.empty(); iter.next()) {
    bp = iter->value;
    fname = image->LookupFile(bp->addr());
    number++;

    // Correct file?
    if (fname != nullptr && !strcmp(fname, filename)) {
      // A function breakpoint
      if (bp->name() != nullptr && !strcmp(breakpoint, bp->name()))
        return number;

      // Line breakpoint
      if (image->LookupLine(bp->addr(), &line) &&
        line == strtoul(breakpoint, NULL, 10) - 1)
        return number;
    }
  }
  return -1;
}

void
Debugger::ListBreakpoints()
{
  LegacyImage *image = context_->runtime()->image();

  Breakpoint *bp;
  uint32_t line;
  const char *filename;
  uint32_t number = 0;
  for (BreakpointMap::iterator iter = breakpoint_map_.iter(); !iter.empty(); iter.next()) {
    bp = iter->value;

    printf("%2d  ", ++number);
    line = bp->line();
    if (line > 0) {
      printf("line: %d", line);
    }

    if (bp->temporary())
      printf("  (TEMP)");

    filename = bp->filename();
    if (filename != nullptr) {
      printf("\tfile: %s", skippath(filename));
    }

    if (bp->name() != nullptr) {
      printf("\tfunc: %s", bp->name());
    }
    printf("\n");
  }
}

char *
Debugger::ParseBreakpointLine(char *input, const char **filename)
{
  LegacyImage *image = context_->runtime()->image();

  // check if a filename precedes the breakpoint location
  char *sep;
  if ((sep = strchr(input, ':')) != nullptr) {
    /* the user may have given a partial filename (e.g. without a path), so
    * walk through all files to find a match
    */
    *sep = '\0';
    *filename = image->FindFileByPartialName(input);
    if (*filename == nullptr) {
      fputs("Invalid filename.\n", stdout);
      return nullptr;
    }

    // Skip colon and settle before line number
    input = sep + 1;
  }

  input = skipwhitespace(input);

  return input;
}

bool
Debugger::AddWatch(const char* symname)
{
  WatchTable::Insert i = watch_table_.findForAdd(symname);
  if (i.found())
    return false;
  watch_table_.add(i, symname);
  return true;
}

bool
Debugger::ClearWatch(const char* symname)
{
  WatchTable::Result r = watch_table_.find(symname);
  if (!r.found())
    return false;
  watch_table_.remove(r);
  return true;
}

bool
Debugger::ClearWatch(uint32_t num)
{
  if (num < 1 || num > watch_table_.elements())
    return false;

  uint32_t index = 1;
  for (WatchTable::iterator iter = WatchTable::iterator(&watch_table_); !iter.empty(); iter.next()) {
    if (num == index++) {
      iter.erase();
      break;
    }
  }
  return true;
}

void
Debugger::ClearAllWatches()
{
  watch_table_.clear();
}

void
Debugger::ListWatches()
{
  SmxV1Image *imagev1 = (SmxV1Image *)selected_context_->runtime()->image();
  uint32_t num = 1;
  AString symname;
  int dim;
  uint32_t idx[sDIMEN_MAX];
  const char *indexptr;
  char *behindname = nullptr;
  for (WatchTable::iterator iter = WatchTable::iterator(&watch_table_); !iter.empty(); iter.next()) {
    symname = *iter;

    ke::AutoPtr<SmxV1Image::Symbol> sym;
    indexptr = strchr(symname.chars(), '[');
    behindname = nullptr;
    dim = 0;
    memset(idx, 0, sizeof(idx));
    // Parse all [x][y] dimensions
    while (indexptr != nullptr && dim < sDIMEN_MAX) {
      if (behindname == nullptr)
        behindname = (char *)indexptr;

      indexptr++;
      idx[dim++] = atoi(indexptr);
      indexptr = strchr(indexptr, '[');
    }

    // End the string before the first [ temporarily, 
    // so GetVariable only looks for the variable name.
    if (behindname != nullptr)
      *behindname = '\0';

    // find the symbol with the smallest scope
    if (imagev1->GetVariable(symname.chars(), cip_, sym)) {
      // Add the [ back again
      if (behindname != nullptr)
        *behindname = '[';

      printf("%d  %-12s ", num++, symname.chars());
      DisplayVariable(sym, idx, dim);
      printf("\n");
    }
    else {
      printf("%d  %-12s (not in scope)\n", num++, symname.chars());
    }
  }
}

bool
Debugger::GetSymbolValue(const SmxV1Image::Symbol* sym, int index, cell_t* value)
{
  cell_t *vptr;
  cell_t base = sym->addr();
  if (sym->vclass() & DISP_MASK)
    base += frm_; // addresses of local vars are relative to the frame

  // a reference
  if (sym->ident() == sp::IDENT_REFERENCE || sym->ident() == sp::IDENT_REFARRAY) {
    if (selected_context_->LocalToPhysAddr(base, &vptr) != SP_ERROR_NONE)
      return false;

    assert(vptr != nullptr);
    base = *vptr;
  }

  if (selected_context_->LocalToPhysAddr(base + index*sizeof(cell_t), &vptr) != SP_ERROR_NONE)
    return false;

  if (vptr != nullptr)
    *value = *vptr;
  return vptr != nullptr;
}

bool
Debugger::SetSymbolValue(const SmxV1Image::Symbol* sym, int index, cell_t value)
{
  cell_t *vptr;
  cell_t base = sym->addr();
  if (sym->vclass() & DISP_MASK)
    base += frm_; // addresses of local vars are relative to the frame

  // a reference
  if (sym->ident() == sp::IDENT_REFERENCE || sym->ident() == sp::IDENT_REFARRAY) {
    selected_context_->LocalToPhysAddr(base, &vptr);
    assert(vptr != nullptr);
    base = *vptr;
  }

  selected_context_->LocalToPhysAddr(base + index*sizeof(cell_t), &vptr);
  assert(vptr != nullptr);
  *vptr = value;
  return true;
}

bool
Debugger::SetSymbolString(const SmxV1Image::Symbol* sym, char* str)
{
  assert(sym->ident() == sp::IDENT_ARRAY || sym->ident() == sp::IDENT_REFARRAY);
  assert(sym->dimcount() == 1);

  SmxV1Image *image = (SmxV1Image *)selected_context_->runtime()->image();

  cell_t *vptr;
  cell_t base = sym->addr();
  if (sym->vclass() & DISP_MASK)
    base += frm_; // addresses of local vars are relative to the frame

  // a reference
  if (sym->ident() == sp::IDENT_REFERENCE || sym->ident() == sp::IDENT_REFARRAY) {
    selected_context_->LocalToPhysAddr(base, &vptr);
    assert(vptr != nullptr);
    base = *vptr;
  }

  ke::AutoPtr<ke::Vector<SmxV1Image::ArrayDim *>> dims;
  dims = image->GetArrayDimensions(sym);
  return selected_context_->StringToLocalUTF8(base, dims->at(0)->size(), str, NULL) == SP_ERROR_NONE;
}

char *
Debugger::GetString(SmxV1Image::Symbol* sym)
{
  assert(sym->ident() == sp::IDENT_ARRAY || sym->ident() == sp::IDENT_REFARRAY);
  assert(sym->dimcount() == 1);

  // get the starting address and the length of the string
  cell_t *addr;
  cell_t base = sym->addr();
  if (sym->vclass())
    base += frm_; // addresses of local vars are relative to the frame
  if (sym->ident() == sp::IDENT_REFARRAY) {
    selected_context_->LocalToPhysAddr(base, &addr);
    assert(addr != nullptr);
    base = *addr;
  }

  char *str;
  if (selected_context_->LocalToStringNULL(base, &str) != SP_ERROR_NONE)
    return nullptr;
  return str;
}

void
Debugger::PrintValue(long value, int disptype)
{
  switch (disptype)
  {
  case DISP_FLOAT:
    printf("%f", sp_ctof(value));
    break;
  case DISP_HEX:
    printf("%lx", value);
    break;
  case DISP_BOOL:
    switch (value)
    {
    case 0:
      fputs("false", stdout);
      break;
    case 1:
      fputs("true", stdout);
      break;
    default:
      printf("%ld (false)", value);
      break;
    }
    break;
  default:
    printf("%ld", value);
    break;
  }
}

void
Debugger::DisplayVariable(SmxV1Image::Symbol *sym, uint32_t index[], int idxlevel)
{
  cell_t value;
  ke::AutoPtr<ke::Vector<SmxV1Image::ArrayDim *>> symdims;
  SmxV1Image *image = (SmxV1Image *)selected_context_->runtime()->image();

  assert(index != NULL);

  // first check whether the variable is visible at all
  if ((uint32_t)cip_ < sym->codestart() || (uint32_t)cip_ > sym->codeend()) {
    fputs("(not in scope)", stdout);
    return;
  }

  // set default display type for the symbol (if none was set)
  if ((sym->vclass() & ~DISP_MASK) == 0) {
    const char *tagname = image->GetTagName(sym->tagid());
    if (tagname != nullptr) {
      if (!stricmp(tagname, "bool")) {
        sym->setVClass(sym->vclass() | DISP_BOOL);
      }
      else if (!stricmp(tagname, "float")) {
        sym->setVClass(sym->vclass() | DISP_FLOAT);
      }
    }
    if ((sym->vclass() & ~DISP_MASK) == 0 &&
      (sym->ident() == sp::IDENT_ARRAY || sym->ident() == sp::IDENT_REFARRAY) &&
      sym->dimcount() == 1)
    {
      /* untagged array with a single dimension, walk through all elements
      * and check whether this could be a string
      */
      char *ptr = GetString(sym);
      if (ptr != nullptr)
      {
        uint32_t i;
        for (i = 0; i < MAXLINELENGTH - 1 && ptr[i] != '\0'; i++) {
          if ((ptr[i] < ' ' && ptr[i] != '\n' && ptr[i] != '\r' && ptr[i] != '\t') ||
            ptr[i] >= 128)
            break; // non-ASCII character
          if (i == 0 && !isalpha(ptr[i]))
            break; // want a letter at the start
        }
        if (i > 0 && i < MAXLINELENGTH - 1 && ptr[i] == '\0')
          sym->setVClass(sym->vclass() | DISP_STRING);
      }
    }
  }

  if (sym->ident() == sp::IDENT_ARRAY || sym->ident() == sp::IDENT_REFARRAY) {
    int dim;
    symdims = image->GetArrayDimensions(sym);
    // check whether any of the indices are out of range
    assert(symdims != nullptr);
    for (dim = 0; dim < idxlevel; dim++) {
      if (symdims->at(dim)->size() > 0 && index[dim] >= symdims->at(dim)->size())
        break;
    }
    if (dim < idxlevel) {
      fputs("(index out of range)", stdout);
      return;
    }
  }

  if ((sym->ident() == sp::IDENT_ARRAY || sym->ident() == sp::IDENT_REFARRAY) &&
    idxlevel == 0)
  {
    if ((sym->vclass() & ~DISP_MASK) == DISP_STRING) {
      char *str = GetString(sym);
      if (str != nullptr)
        printf("\"%s\"", str); // TODO: truncate to 40 chars
      else
        fputs("NULL_STRING", stdout);
    }
    else if (sym->dimcount() == 1) {
      assert(symdims != nullptr); // set in the previous block
      uint32_t len = symdims->at(0)->size();
      if (len > 5)
        len = 5;
      else if (len == 0)
        len = 1; // unknown array length, assume at least 1 element

      fputs("{", stdout);
      uint32_t i;
      for (i = 0; i < len; i++) {
        if (i > 0)
          fputs(",", stdout);
        if (GetSymbolValue(sym, i, &value))
          PrintValue(value, (sym->vclass() & ~DISP_MASK));
        else
          fputs("?", stdout);
      }
      if (len < symdims->at(0)->size() || symdims->at(0)->size() == 0)
        fputs(",...", stdout);
      fputs("}", stdout);
    }
    else {
      fputs("(multi-dimensional array)", stdout);
    }
  }
  else if (sym->ident() != sp::IDENT_ARRAY && sym->ident() != sp::IDENT_REFARRAY && idxlevel > 0) {
    // index used on a non-array
    fputs("(invalid index, not an array)", stdout);
  }
  else {
    // simple variable, or indexed array element
    assert(idxlevel > 0 || index[0] == 0); // index should be zero if non-array
    int dim;
    int base = 0;
    for (dim = 0; dim < idxlevel - 1; dim++) {
      base += index[dim];
      if (!GetSymbolValue(sym, base, &value))
        break;
      base += value / sizeof(cell_t);
    }

    if (GetSymbolValue(sym, base + index[dim], &value) &&
      sym->dimcount() == idxlevel)
      PrintValue(value, (sym->vclass() & ~DISP_MASK));
    else if (sym->dimcount() != idxlevel)
      fputs("(invalid number of dimensions)", stdout);
    else
      fputs("?", stdout);
  }
}

void
Debugger::DumpStack()
{
  FrameIterator &iter = *frames_;

  iter.Reset();

  uint32_t index = 0;
  for (; !iter.Done(); iter.Next(), index++) {
    if (index == selected_frame_) {
      fputs("->", stdout);
    }
    else {
      fputs("  ", stdout);
    }
    
    const char *name = iter.FunctionName();
    if (!name) {
      name = "<unknown function>";
    }

    if (iter.IsNativeFrame()) {
      fprintf(stdout, "[%d] %s", index, name);
      continue;
    }

    if (iter.IsScriptedFrame()) {
      const char *file = iter.FilePath();
      if (!file)
        file = "<unknown>";
      fprintf(stdout, "[%d] Line %d, %s::%s\n", index, iter.LineNumber(), file, name);
    }
  }
}

} // namespace sp

namespace SourcePawn {

using namespace sp;

bool
ConsoleDebugger::IsEnabled() {
  return enabled_;
}

bool
ConsoleDebugger::SetEnabled(bool enable) {
  ke::AutoLock lock(Environment::get()->lock());
  if (Environment::get()->HasRuntimesRegistered())
    return false;

  enabled_ = enable;
  return true;
}

bool
ConsoleDebugger::StartDebugger(const IPluginContext *ctx) {
  PluginContext *context = (PluginContext *)ctx;
  return context->StartDebugger();
}

ke::Vector<IBreakpoint *> *
ConsoleDebugger::GetBreakpoints(const IPluginContext *ctx) {
  PluginContext *context = (PluginContext *)ctx;
  Debugger *debugger = context->GetDebugger();
  
  if (!debugger->active())
    return nullptr;

  ke::Vector<IBreakpoint *> *breakpoints = new ke::Vector<IBreakpoint *>();
  Breakpoint *bp;
  for (Debugger::BreakpointMap::iterator iter = debugger->breakpoint_map_.iter(); !iter.empty(); iter.next()) {
    bp = iter->value;

    breakpoints->append((IBreakpoint *)bp);
  }
  return breakpoints;
}

IBreakpoint *
ConsoleDebugger::AddBreakpoint(const IPluginContext *ctx, const char *line, bool temporary) {
  PluginContext *context = (PluginContext *)ctx;
  Debugger *debugger = context->GetDebugger();

  if (!debugger->active())
    return nullptr;

  // check if a filename precedes the breakpoint location
  const char *filename = nullptr;
  line = debugger->ParseBreakpointLine((char *)line, &filename);

  // User didn't specify a filename. 
  // Use the main source file by default (last one in the list).
  if (!filename) {
    SmxV1Image *imagev1 = (SmxV1Image *)context->runtime()->image();
    if (imagev1->GetFileCount() <= 0)
      return nullptr;
    filename = imagev1->GetFileName(imagev1->GetFileCount() - 1);
  }

  Breakpoint *bp;
  // User specified a line number
  if (isdigit(*line)) {
    bp = debugger->AddBreakpoint(filename, strtol(line, NULL, 10) - 1, temporary);
  }
  // User specified a function name
  else {
    bp = debugger->AddBreakpoint(filename, line, temporary);
  }
  return (IBreakpoint *)bp;
}

bool
ConsoleDebugger::ClearBreakpoint(const IPluginContext *ctx, int bpnum) {
  PluginContext *context = (PluginContext *)ctx;
  Debugger *debugger = context->GetDebugger();

  if (!debugger->active())
    return false;

  return debugger->ClearBreakpoint(bpnum);
}

} // namespace SourcePawn