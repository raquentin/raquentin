set PROJECT_NAME "clash_audio_dynamic_gain"

set PROJECT_DIR "./project"

set VERILOG_DIR "../../verilog/Amplifiers.DynamicGain.topEntity"

set TOP_MODULE "topEntity"

set CONSTRAINTS_FILE "./constraints.xdc"

set FPGA_PART "xc7z020clg400-1"

create_project $PROJECT_NAME $PROJECT_DIR -part $FPGA_PART -force

add_files -norecurse $VERILOG_DIR/topEntity.v

add_files $CONSTRAINTS_FILE

set_property top $TOP_MODULE [current_fileset]

set_property target_language Verilog [current_fileset]
