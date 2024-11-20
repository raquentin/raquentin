# 🔊 clash-audio
 An open-source IP library for processing audio streams on hardware with Haskell.

<img src="https://github.com/user-attachments/assets/e44424ce-3cfe-4e47-8261-de5741f3e35e" width=40%>

## Running The Project

### Generating Verilog

- `cabal run clashi`
- `clashi> :l src/<core you want to transpile>`
- `clashi> :verilog`

Verilog entity will be written to `/verilog/<CoreType>.<CoreName>.topEntity/<CoreName>.v`.

### Creating AMD Xilinx Project Files

See `/examples` with constraints and Tcl files. These files generate Vivado project files to show how different cores work.
