# nes_unit_testing.lisp

Tiny unit testing for NES homebrew. See
[calculator.nes](https://github.com/dustmop/calculator) for usage.


# Reference

deftest <name>
- Macro that defines a test case with name.

initialize-test-case <:env env> <:filter-source filter-source>
- Initialize the test case by compiling the source subroutine, clearing ram, and creating labels in the env.
- env An environment, list of (name value), to create labels from, see "Environments" below. Names cannot be numbers, only symbols. Values can be ":byte" or ":word" to create labels at an arbitrary address of the respective size. Values can be a number to create labels at that address.
- filter-source An optional function to be used to filter more lines from the source code.

initialize-fake-ppu
- Clear the list of ppu updates.

run-test-case <:a a> <:x x> <:y y> <:memory memory>
- Assign the given values, then run the assembled code.
- a x y, Values to assign to registers A, X, Y.
- memory An environment, list of (name value) to assign to memory, see "Environments" below. If the name is a variable, the value should be a single number. If the name is a number, the value should be a list of numbers.

expect-result <:a a> <:x x> <:y y> <:memory memory> <:ppu ppu>
- Verify that the given values match the state after the test case has run.
- a x y, Values that should be in the registers A, X, Y.
- memory An environment, list of (name value) to verify, see "Environments" below. If the name is a variable, the value should be a single number. If the name is a number, the value should be a list of numbers.
- ppu Values that should have been sent to the ppu, list of (name value). Each name should be a number, equal to the ppu pointer, and the value should be a number, equal to the data in the ppu ram.

get-cpu-state <kind>
- Get a register value, if given ":a", ":x", ":y", or the cycle count if given ":cc".

display-test-timing
- Display to standard output the timing of the test runs.


# Environments

A list of tuples (name value). Name can be a symbol, which will be lower-cased and have dashes converted to underscores, or a number. The value can be ":byte" or ":word" for initialize-test-case only. Otherwise, the value can be a number of list of numbers.


# Examples
```
  ; Declare variables "some_variable" and "another_variable", 1 byte and 2
  ; bytes each. Define "table_data" to be equal to $6000.
  (initialize-test-case :env '((some-variable :byte)
                               (another-variable :word)
                               (table-data #x6000)))
  ; Set "some_variable" to 16, "another_variable" to 32, and put the bytes
  ; 10, 0, 10 at address $0300.
  (run-test-case :memory '((some-varible 16)
                           (another-variable 32)
                           (#x0300 (10 0 10))))
  ; Verify that "some_variable" is equal to 160, and the bytes 12, 2, 13 are
  ; at address $0300. Verify that the ppu had its pointer be set to $23f0 and
  ; got the value 15.
  (expect-result :memory '((some-variable 160)
                           (#x0300 (12 2 12)))
                 :ppu '((#x23f0 15)))
```
