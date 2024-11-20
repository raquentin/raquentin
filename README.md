# 🔊 clash-audio
 An open-source IP library for processing audio streams on hardware with Haskell.

<img src="https://github.com/user-attachments/assets/e44424ce-3cfe-4e47-8261-de5741f3e35e" width=40%>

## Running The Project

### Running Tests

- `cabal run clashi`
- `clashi> :l tests/Tests/<core you want to test>`
- `clashi> sampleN <number of samples> tb`

You'll want to use `sampleN` to finitely sample the output wire of the circuit. Otherwise your console will be filled with `True` until you interrupt. The output of tb is True if all specified stimuli are exhausted, not whether the tests "pass" really, since the signal holds a value indefinetly. If a test's expected output doesn't match an input, you'll see the error below:

```
cycle(<Clock: System>): 4, outputVerifier
expected value: 9, not equal to actual value: 8
```

In this example, the expected value didn't match the actual output on clock cycle 4.

### Generating Verilog

- `cabal run clashi`
- `clashi> :l src/<core you want to transpile>`
- `clashi> :verilog`

Verilog entity will be written to `/verilog/<CoreType>.<CoreName>.topEntity/<CoreName>.v`.

### Creating AMD Xilinx Project Files

See `/examples` with constraints and Tcl files. These files generate Vivado project files to show how different cores work.
