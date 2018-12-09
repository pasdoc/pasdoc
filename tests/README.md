# Tests for PasDoc

Run all the tests simply by `run_all_tests.sh`.
This runs all relevant tests described below.
It requires a Unix shell (on Windows, you should install Cygwin or MSys).

# Testcases

The subdirectory `testcases` contains
some short and difficult Pasdoc sources. Some of these are taken
from bugreports, to make sure we don't reintroduce an old bug again.
Some of these are written to test new features.

To add a new testcase,

1. add the file to `testcases` subdirectory
   (follow the naming conventions below, usually just ok_xxx.pas).
2. add an appropriate line at the end of scripts/mk_tests.sh.
3. after you make sure it works, commit the new version of `testcases_output`.

Using these tests fully requires having some standard Unix tools
installed and available on $PATH. You need GNU `make`, `bash`, `diff`.

And you need the `pasdoc` binary available on $PATH.

## Naming of Pascal unit files in testcases subdirectory

- `ok_*`

    Means that this unit should be parsed by pasdoc without any warnings.

- `warning_*`

    Means that this unit should be parsed by pasdoc (i.e. some documentation
    for it should be generated), but some warnings should be reported
    by pasdoc.

- `error_*`

    Means that pasdoc should not be able to parse this unit,
    documentation for this unit shouldn't be possible to generate.
    pasdoc should generate proper error message for this case.

    Note that pasdoc may report errors as warnings,
    e.g. "Warning[2]: Error EPasDoc: error_line_number.pas(26):
    could not open include file not_existing.inc ..."
    Pasdoc calls this a warning, since, after all, it can continue
    it's work by simply skipping to the next unit.
    But for the sake of this distinction, this is an *error*,
    not merely a *warning*.
    The precise difference between an error and a warning is:
    "error makes impossible to generate documentation for this unit".

Units are divided into these 3 groups because:

- These groups are precisely defined, so there shouldn't be any concern
  about "where this test unit belongs".

- We should be eventually able to use output messages and exit status
  of pasdoc to automate testing as much as we can.

Notes:

- Please keep prefixes "ok_", "warning_", "error_" lowercase so that
  e.g. the file-mask `ok_*` works as expected on case-sensitive file-systems.
  Try to follow the convention
  "prefix_description_of_test_lowercase_with_underscores.pas".

- Most of these units will have empty implementation.
  They are not supposed to be ever compiled by anything.

- There is no requirement here that the interface of units
  placed here must be correct Pascal code.
  Pasdoc should be able to handle even incorrect units.
  Usually it should break with some parsing error on such units,
  but it doesn't have to (after all, pasdoc is not meant to
  exactly reimplement some part of a compiler that carefully checks
  syntax and everything), it may try to generate some docs.
  But even with incorrect units, it obviously shouldn't fail with some
  miserable error (like sigsegv :) or do anything that it shouldn't,
  like removing some files or so.

- If you want to test unit with a special pasdoc's command-line,
  you must add appropriate line at the end of ./mk_tests.sh script.

## Possible tests to be done

First of all, you can just run `run_all_tests.sh` that runs all
relevant tests described below.

By default `run_all_tests.sh` assumes that you are inside a GIT repository
(cloned from https://github.com/pasdoc/pasdoc ), and uses `git diff`
to compare. If you have sources from the tarball, then define
environment variable `USE_DIFF_TO_COMPARE` to `true` before running, like this:

```
export USE_DIFF_TO_COMPARE=true
./run_all_tests.sh
```

- Regenerate `testcases_output` contents:

    ```
    cd testcases/
    ../scripts/mk_tests.sh html
    # or regenerate all:
    ../scripts/mk_tests.sh html htmlhelp latex latex2rtf simplexml
    ```

    Tests are created in `testcases_output/html` subdirectory.

    You can just manually look at units'
    sources and comments there to know what generated documentation should
    look like (in case of `ok_*` and `warning_*` files) and what
    warnings/errors should be reported (in case of `warning_*` and `error_*` files).

    Of course, even briefly checking that all `ok_*` units generate no warnings,
    all `warning_*` units generate some warnings (and produce some docs)
    and all `error_*` units generate errors (and no docs)
    is still a better test than nothing...

    Note that pasdoc messages (printed on stdout) will not be shown
    when you will make tests using mentioned `make ...' commands.
    Instead they will be saved to files named PASDOC-OUTPUT
    in appropriate subdirectories of tests output.
    This way we treat messages printed by pasdoc as important part
    of pasdoc's output, they are included in "correct tests output"
    (see below) so comparing with "correct tests output" also checks
    that pasdoc messages are correct (warnings are as they should be
    and where they should be etc.)

- Since PasDoc version 0.16.0, we keep the correct state of `testcases_output`
  inside the code repository.

    To make sure that nothing is broken, simply check that after regenerating
    the `testcases_output`, their contents are not changed.
    Use `git status`, `git diff` or any visual tool on top of them that you like.

    And if the change is desired (e.g. you added a new feature that changes
    existing output), just commit them.

    (Side discussion:
    Why do we keep `testcases_output` inside the code repository?
    Why not ignore them?

    This is a little abuse of the version control system idea
    (as we keep auto-generated files inside code repository),
    but it's very useful. This way our `testcases_output`
    are versioned (each PasDoc source code state
    has a corresponding testcases_output state recorded),
    and it's easy to compare them (just use `git diff`) and accept new state
    (just commit the changes).

    We used to have a complicated system of packing and uploading/downloading
    the correct tests output, and it was more trouble than it was worth.
    )

- `scripts/validate_html.sh`

   This is an automatic test that makes html docs for all test units
   and validates them using v.Nu validator.

   For this to work, vnu has to be available at $PATH. This can be archieved
   by downloading the most recent version from [here](https://github.com/validator/validator/releases)
   and then adding the following wrapper script to your bin directory:

   ```
   #!/bin/bash
   exec java  -jar /path/to/vnu.jar "$@"
   ```

   On macOS, v.Nu can be installed using Homebrew simply by running `brew install vnu`.

- `scripts/validate_simplexml.sh`

   This is an automatic test that makes simplexml docs for all test units
   and validates them using xmllint.
   xmllint must be installed for this to work.

   We do not have any DTD, so it doesn't check that our XML files
   conform to anything. But at least it checks that they are well-formed
   (all open elements are closed and such).

- Cache tests:

    ```
    cd scripts/
   ./check_cache.sh html
   ./check_cache.sh latex
   ```

    $1 arg for this script is required and is any valid pasdoc format
    (as for pasdoc's --format option).
    Rest of args are just passed to pasdoc as they are.

    This runs pasdoc twice using the cache,
    1st time the cache directory is empty and pasdoc writes the cache,
    2nd time pasdoc should read everything from the cache.
    The script then checks that pasdoc's output was identical each time
    (comparing them with `diff -u', so you get diff on output if something
    does not match).

    This somehow checks that writing the cache and reading the cache
    and writing documentation basing on information obtained from the cache
    (instead of from parsing units) works OK.

    Everything is read/written to a temporary directory scripts/check_cache_tmp/,
    that is removed at the beginning and at the end of the script.
    (It's removed at the beginning, and also by `make clean`, just to
    make sure that no garbage is left there, in case script failed
    for whatever reason.)
    So this script is mostly independent from the rest of the tests
    here -- it just happens to use the same test units.

    In case comparison between two outputs failed both outputs
    and left in scripts/check_cache_tmp/, so developer can inspect
    them closer.

- Test cache is format-independent:

    ```
    cd scripts/
    ./check_cache_format_independent.sh html latex
    ./check_cache_format_independent.sh latex html
    ```

    Requires two arguments, two names of pasdoc output format.
    These two formats should be different for this test to be really sensible
    (and better than check_cache.sh), but they can also be equal and
    test should pass anyway.

    This is similar to ./check_cache.sh test, but it checks
    that cache format is independent of pasdoc's output format.
    1st it generates output with format 1, without using any cache.
    2nd it generates output with format 2, writing the cache.
    3rd it generates output with format 1, this time reading the cache.
    Then it checks that 1st and 3rd output are equal.

    This way it checks that cache generated while doing format 2
    may be reused while making format 1. So it tests that cache
    format is really independent from pasdoc's chosen output format.

## Various notes

`make clean` will clean this directory.

Note that make used must be GNU make.
Under Linux this is standard, under FreeBSD this is called `gmake`,
under Win32 you can get this with e.g. FPC, MinGW or Cygwin.

scripts/ subdirectory contains some helpful things for running tests.
These should be scripts that perform some additional tests
on test units available here (like check_cache.sh),
but also some helper scripts/files for Makefile in this directory.

## Subdirectory testcases/todo/

It contains units that are known to
be incorrectly handled by pasdoc by now. "Incorrectly handled"
may mean that generated documentation is incorrect, or that pasdoc
fails with some error on correct input, but "incorrectly handled"
may also mean that pasdoc fails to write a proper warning/error
in case when input (unit's sources) is obviously wrong.

Files inside todo/ should follow exactly the same naming convention
as units in this directory (`ok_*`, `warning_*`, `error_*`).
In this case, unit's name tells what pasdoc *should* do with such unit,
even if it doesn't do it for now.
The idea is that when developer fixes a problem with some unit
in tests/todo/ directory, he can simply move this unit to tests/.

These files are in separate todo/ subdirectory, because otherwise
every time we would like to check our tests we would have to
remember "oh, such-and-such test fails but it's a known problem,
so I can ignore it". This would be troublesome,
because *usually* we will want to test whether we did not break
anything that previously worked, and we will not care that there
are still some unresolved problems in pasdoc.

---

And that's all for now.
Comments are most welcome.
