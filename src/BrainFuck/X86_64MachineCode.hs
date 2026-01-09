module BrainFuck.X86_64MachineCode (compile) where

import BrainFuck.Parse (BrainFuckAST (..))
import Data.Bits (shiftR)
import Data.Word (Word32, Word64, Word8)

tapeLength :: Word32
tapeLength = 30000

compile :: [BrainFuckAST] -> [Word8]
compile ast =
  let pre = prelude
   in let post = postlude
       in let body = concatMap handleNode ast
           in elfHeader ++ programHeader (fromIntegral (length pre + length body + length post + 120)) ++ pre ++ body ++ post

word64ToLE :: Word64 -> [Word8]
word64ToLE w = [fromIntegral (w `shiftR` (8 * i)) | i <- [0 .. 7]]

word32ToLE :: Word32 -> [Word8]
word32ToLE w = [fromIntegral (w `shiftR` (8 * i)) | i <- [0 .. 3]]

elfHeader :: [Word8]
elfHeader =
  [0x7F, 0x45, 0x4C, 0x46] -- ELF
    ++ [0x2] -- (e_ident[EI_CLASS]) 64 bit
    ++ [0x1] -- (e_ident[DATA]) little endian
    ++ [0x1] -- (e_ident[EI_VERSION]) ELF v1
    ++ [0x0] -- (e_ident[EI_OSABI]) System v (aparently this is used rather than linux)
    ++ [0, 0, 0, 0, 0, 0, 0, 0] -- (e_ident[EI_PAD]) Padding
    ++ [0x2, 0] -- (e_type) Executable
    ++ [0x3E, 0] -- (e_machine) x84-64
    ++ [0x1, 0, 0, 0] -- (e_version) ELF v1
    ++ [0x78, 0, 0x40, 0, 0, 0, 0, 0] -- (e_entry) program entry point
    ++ [0x40, 0, 0, 0, 0, 0, 0, 0] -- (e_phoff) program header location
    ++ [0, 0, 0, 0, 0, 0, 0, 0] -- (e_shoff) start of the section header table??
    ++ [0, 0, 0, 0] -- (e_flags)
    ++ [0x40, 0] -- (e_ehsize) size of this header (64 bytes)
    ++ [0x38, 0] -- (e_phentsize) size of the program header table
    ++ [1, 0] -- (e_phnum) one program header
    ++ [0x40, 0] -- (e_shentsize) section header size (64 bytes)
    ++ [0, 0] -- (e_shnum) no section headers
    ++ [0, 0] -- (e_shstrndx) index of section names (we dont have any)

programHeader :: Word64 -> [Word8]
programHeader fileSize =
  [0x1, 0, 0, 0] -- (p_type) PT_LOAD
    ++ [0x7, 0, 0, 0] -- (p_flags) read and execute
    ++ [0, 0, 0, 0, 0, 0, 0, 0] -- (p_offset)
    ++ [0, 0, 0x40, 0, 0, 0, 0, 0] -- (p_vaddr)
    ++ [0, 0, 0, 0, 0, 0, 0, 0] -- (p_paddr)
    ++ word64ToLE fileSize -- (p_filesz)
    ++ word64ToLE fileSize -- (p_memsz)
    ++ [0, 0, 0, 0, 0, 0, 0, 0] -- (p_align) program alignment?

prelude :: [Word8]
prelude =
  [0x48, 0x81, 0xEC]
    ++ word32ToLE (tapeLength + 8) -- sub rsp (tapeLength + 8),
    ++ [0x48, 0xC7, 0x04, 0x24, 0x08, 0, 0, 0] -- mov qword [rsp] 8

postlude :: [Word8]
postlude =
  [0x48, 0x81, 0xC4]
    ++ word32ToLE (tapeLength + 8) -- add rsp (tapeLength + 8)
    ++ [0xB8, 0x3C, 0, 0, 0] -- mov eax 0x3C
    ++ [0x48, 0x31, 0xff] -- xor rdi, rdi
    ++ [0x0F, 0x05] -- syscall

compileNodes :: [BrainFuckAST] -> [Word8]
compileNodes = concatMap handleNode

handleNode :: BrainFuckAST -> [Word8]
handleNode (PtrArithmetic n) =
  [0x48, 0x81, 0x04, 0x24] ++ word32ToLE (fromIntegral n) -- add qword [rsp], n
handleNode (DataArithmetic n) =
  [0x48, 0x8B, 0x04, 0x24] -- move rax, [rsp]
    ++ [0x80, 0x04, 0x04]
    ++ [fromIntegral n] -- add byte [rsp + rax], n
handleNode ClearCell =
  [0x48, 0x8B, 0x04, 0x24] -- mov rax, [rsp]
    ++ [0xC6, 0x04, 0x04, 0x0] -- mov byte [rsp+rax], 0
handleNode GetChar =
  [0x4C, 0x8B, 0x14, 0x24] -- mov r10, [rsp]
    ++ [0x4A, 0x8D, 0x34, 0x14] -- lea rsi, [rsp + r10]
    ++ [0xB8, 0, 0, 0, 0] -- mov rax, 0
    ++ [0xBD, 0, 0, 0, 0] -- mov rdi, 0
    ++ [0xBA, 0x01, 0, 0, 0] -- mov rdx, 1
    ++ [0x0F, 0x05] -- syscall
handleNode PutChar =
  [0x4C, 0x8B, 0x14, 0x24] -- mov r10, [rsp]
    ++ [0x4A, 0x8D, 0x34, 0x14] -- lea rsi, [rsp + r10]
    ++ [0xB8, 0x01, 0, 0, 0] -- mov rax, 1
    ++ [0xBD, 0x01, 0, 0, 0] -- mov rdi, 1
    ++ [0xBA, 0x01, 0, 0, 0] -- mov rdx, 1
    ++ [0x0F, 0x05] -- syscall
handleNode (Loop body) =
  let bodyCode = compileNodes body
   in let bodyCodeLen = fromIntegral (length bodyCode) :: Word32
       in [0x4C, 0x8B, 0x14, 0x24] -- mov r10, [rsp]
            ++ [0x42, 0x8A, 0x04, 0x14] -- mov al, [rsp + r10]
            ++ [0x84, 0xC0] -- test al, al
            ++ [0x0F, 0x84]
            ++ word32ToLE (bodyCodeLen + 5) -- jump if zero to end of body
            ++ bodyCode
            ++ [0xE9]
            ++ word32ToLE (negate (bodyCodeLen + 21)) -- jump back
