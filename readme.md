# Another C Preprocessor

This project is for some Compiler Principle lecture homework, which need a fully implanted C preprocessor of C23 standard. The target of this project is to eventually provide a cli tool to preform [Translation Phase 1-6](https://en.cppreference.com/w/c/language/translation_phases) described in standard.

This project will eventually implant below features:

1. **Phases 1 & 2**: a character feeder function `bytes -> int -> int`, accepts:
   1. `bbuf`: a bytes buffer,
   2. `bcnt`: an integer of count of demanded input characters.
   3. *return*: the count of actually inputed characters, must less equal to `bcnt`.
2. **Phase 3**: the preprocessor lexer, outputs preprocessor tokens.
3. **Phase 4**: the preprocessor parser, manages preprocessor context and executes preprocessor instructions. Executed instructs will be removed and replaced with an `\n`. For `#include` and `#embed`, an line marker will be added.
4. **Phases 5 & 6**: output to file/channel.

Translation Phases according to this implantation:

1. **Translate Phase 1**: 
   1. The individual bytes of the source code file are mapped to single byte representations. However, this project doesn't consider UTF-8 characters, though we belive it should behave well, too.
   2. All available bytes are considered legal source characters, for we don't want to handle UTF-8 characters separately.
   3. Trigraph sequences are not supported, but digraphs are supported.
2. **Translate Phase 2**:
   1. Whenever a `\\\n` sequence presents, delete this sequence and concatenate next line into current one, add one `\n` to the end of current line.^[So we preserved the line numbers after concatenation.]
   2. If a non-empty source file had no newline originally, or ended with a backslash, consider the next line as an empty line with only `\n`.
3. **Translate Phase 3**:
   1. The source file is decomposed into comment, sequences of whitespace characters (space, horizontal tab, new-line, vertical tab, and form-feed), and preprocessing tokens, which are the following:
      1. header names: <stdio.h> or "myfile.h"
      2. identifiers
      3. preprocessing numbers, which cover integer constants and floating constants, but also cover some invalid tokens such as 1..E+3.foo or 0JBK
      4. character constants and string literals
      5. operators and punctuators, such as +, <<=, <%, or ##.
      6. individual non-whitespace characters that do not fit in any other category
   2. Each comment is replaced by one space character
   3. Newlines are kept, and it's implementation-defined whether non-newline whitespace sequences may be collapsed into single space characters.
4. **Translate Phase 4**:
   1. Preprocessor is executed.
   2. Each file introduced with the #include directive goes through phases 1 through 4, recursively.
   3. At the end of this phase, all preprocessor directives are removed from the source.
5. **Translate Phase 5**:
   1. All characters and escape sequences in character constants and string literals are converted from source character set to execution character set (which may be a multibyte character set such as UTF-8, as long as all 96 characters from the basic source character set listed in phase 1 have single-byte representations). If the character specified by an escape sequence isn't a member of the execution character set, the result is implementation-defined, but is guaranteed to not be a null (wide) character.
6. **Translate Phase 6**:
   1. Adjacent string literals are concatenated.

