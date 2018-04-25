// scaffold main() to support Tiger compilers with gnu-toolchain and qemu for RISCV-32G

// your compiler should emit an assembly-language file with a 'globl' called tigermain taht returns an integer
extern int tigermain();

int main() {
	return tigermain();
}
