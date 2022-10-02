#PATHS
VEXRISCV_DIR ?= $(shell pwd)
VEXRISCV_SRC_DIR=$(VEXRISCV_DIR)/hardware/src
VEX_SOFTWARE_DIR=$(VEXRISCV_DIR)/software
VEX_OS_DIR=$(VEX_SOFTWARE_DIR)/OS_build
VEX_SUBMODULES_DIR=$(VEXRISCV_DIR)/submodules

#RISC-V HARD MULTIPLIER AND DIVIDER INSTRUCTIONS
USE_MUL_DIV ?=1

#RISC-V ATOMIC INSTRUCTIONS
USE_ATOMIC ?=1

#RISC-V COMPRESSED INSTRUCTIONS
USE_COMPRESSED ?=1
