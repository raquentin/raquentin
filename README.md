# 🔊 clash-audio
 An open-source IP library for processing audio streams on hardware with Haskell.

## Running The Project

### Generating Verilog

- `cabal run clashi`
- `clashi> :l src/<core you want to transpile>`
- `:verilog`

Verilog entity will be written to `/verilog/<CoreType>.<CoreName>.topEntity/<CoreName>.v`.

### Creating AMD Xilinx Project Files

See `/examples` with constraints and Tcl files. These files generate Vivado project files to show how different cores work.
