package tinycc.cli

import tinycc.backend.t86.T86Utils

case class CodegenOptions(optimize: Boolean = false,
                          registerCnt: Int = T86Utils.defaultMachineRegCount,
                          floatRegisterCnt: Int = T86Utils.defaultMachineFRegCount)
