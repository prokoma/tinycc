package tinycc.backend

/**
 * The purpose of a register allocator is to map virtual registers to real machine registers and backup caller- and callee- saved registers.
 * Some registers can overflow available registers, in that case it should generate code to spill them into memory.
 */
abstract class RegisterAllocator[T](program: T) {

}
