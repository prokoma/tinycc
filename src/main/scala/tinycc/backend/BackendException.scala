package tinycc.backend

import tinycc.ProgramException

class BackendException(message: String, cause: Throwable = null) extends ProgramException(message, cause)

