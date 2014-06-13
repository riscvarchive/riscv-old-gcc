/*======================================================================*/
/* Default maven linker script                                          */
/*======================================================================*/
/* This is the default linker script for maven. It is based off of the
   mips idt32.ld linker script. I have added many more comments and
   tried to clean things up a bit. For more information about standard
   MIPS sections see Section 9.5 of "See MIPS Run Linux" by Dominic
   Sweetman. For more generic information about the init, fini, ctors,
   and dtors sections see the paper titled "ELF From the Programmers
   Perspective" by Hongiu Lu. */

/*----------------------------------------------------------------------*/
/* Setup                                                                */
/*----------------------------------------------------------------------*/

/* The OUTPUT_ARCH command specifies the machine architecture where the
   argument is one of the names used in the BFD library. More
   specifically one of the entires in bfd/cpu-mips.c */

OUTPUT_ARCH( "riscv" )

/* The ENTRY command specifies the entry point (ie. first instruction to
   execute). The symbol _start is defined in crt0.S */

ENTRY( _start )

/* The GROUP command is special since the listed archives will be
   searched repeatedly until there are no new undefined references. We
   need this since -lc depends on -lgloss and -lgloss depends on -lc. I
   thought gcc would automatically include -lgcc when needed, but
   idt32.ld includes it explicitly here and I was seeing link errors
   without it. */

GROUP( -lc -lgloss -lgcc )

/* The STARTUP command specifes any additional startup files which
   should be always linked _first_ into the executable. */

STARTUP( crt0.o )

/*----------------------------------------------------------------------*/
/* Sections                                                             */
/*----------------------------------------------------------------------*/
/* This is where we specify how the input sections map to output
   sections. The .= commands set the location counter, and the
   sections are inserted in increasing address order according to the
   location counter. The following statement will take all of the .bar
   input sections and reloate them into the .foo output section which
   starts at address 0x1000.

    . = 0.x1000;
    .foo : { *(.bar) }

   If we wrap an input specification with a KEEP command then it
   prevents it from being eliminted during "link-time garbage
   collection". I'm not sure what this is, so I just followed what was
   done in idt32.ld.

   We can also set a global external symbol to a specific address in the
   output binary with this syntax:

    _etext = .;
    PROVIDE( etext = . );

   This will set the global symbol _ftext to the current location. If we
   wrap this in a PROVIDE commad, the symbol will only be set if it is
   not defined. We do this with symbols which don't begin with an
   underscore since technically in ansi C someone might have a function
   with the same name (eg. etext).

   If we need to label the beginning of a section we need to make sure
   that the linker doesn't insert an orphan section inbetween where we
   set the symbol and the actual begining of the section. We can do that
   by assigning the location dot to itself.

    . = .
    _ftext = .;
    .text :
    { }

   */

SECTIONS
{

  /*--------------------------------------------------------------------*/
  /* Code and read-only segment                                         */
  /*--------------------------------------------------------------------*/

  /* Begining of code and text segment */
  . = 0x00010000;
  _ftext = .;
  PROVIDE( eprol = . );

  /* text: Program code section */
  .text : 
  {
    *(.text)
    *(.text.*)
    *(.gnu.linkonce.t.*)
  }

  /* init: Code to execute before main (called by crt0.S) */
  .init : 
  {
    KEEP( *(.init) )
  }

  /* fini: Code to execute after main (called by crt0.S) */
  .fini : 
  {
    KEEP( *(.fini) )
  }

  /* rodata: Read-only data */
  .rodata : 
  {
    *(.rdata)
    *(.rodata)
    *(.rodata.*)
    *(.gnu.linkonce.r.*)
  }

  /* End of code and read-only segment */
  PROVIDE( etext = . );
  _etext = .;

  /*--------------------------------------------------------------------*/
  /* Global constructor/destructor segement                             */
  /*--------------------------------------------------------------------*/
  /* The .ctors/.dtors sections are special sections which contain a
     list of constructor/destructor function pointers. crtbegin.o
     includes code in a .init section which goes through the .ctors list
     and calls each constuctor. crtend.o includes code in a .fini
     section which goes through the .dtors list and calls each
     destructor. crtbegin.o includes a special null pointer in its own
     .ctors/.dtors sections which acts as a start indicator for those
     lists. crtend.o also includes a special null pointer in its own
     .ctors/.dtors sections which acts as an end indictor. The linker
     commands below are setup so that crtbegin.o's .ctors/.dtors
     sections are always first and crtend.o's .ctors/.dtors sections are
     always last. This is the only way the list of functions will have
     the begin and end indicators in the right place. */

  /* ctors : Array of global constructor function pointers */
  .ctors :
  {
    KEEP( *crtbegin.o(.ctors) )
    KEEP( *(EXCLUDE_FILE(*crtend.o) .ctors) )
    KEEP( *(SORT(.ctors.*)) )
    KEEP( *(.ctors) )
  }

  /* dtors : Array of global destructor function pointers */
  .dtors :
  {
    KEEP( *crtbegin.o(.dtors) )
    KEEP( *(EXCLUDE_FILE(*crtend.o) .dtors) )
    KEEP( *(SORT(.dtors.*)) )
    KEEP( *(.dtors) )
  }

  /*--------------------------------------------------------------------*/
  /* Other misc gcc segments (this was in idt32.ld)                     */
  /*--------------------------------------------------------------------*/
  /* I am not quite sure about these sections but it seems they are for
     C++ exception handling. I think .jcr is for "Java Class
     Registration" but it seems to end up in C++ binaries as well. */

  .eh_frame_hdr     : { *(.eh_frame_hdr)     }
  .eh_frame         : { KEEP( *(.eh_frame) ) }
  .gcc_except_table : { *(.gcc_except_table) }
  .jcr              : { KEEP (*(.jcr))       } 

  /*--------------------------------------------------------------------*/
  /* Initialized data segment                                           */
  /*--------------------------------------------------------------------*/

  /* Start of initialized data segment */
  . = ALIGN(16);
   _fdata = .;

  /* data: Writable data */
  .data : 
  {
    *(.data)
    *(.data.*)
    *(.gnu.linkonce.d.*)
  }

  /* End of initialized data segment */
  PROVIDE( edata = . );
  _edata = .;

  /* Have _gp point to middle of sdata/sbss to maximize displacement range */
  . = ALIGN(16);
  _gp = . + 0x800;

  /* Writable small data segment */
  .sdata : 
  {
    *(.sdata)
    *(.sdata.*)
    *(.srodata.*)
    *(.gnu.linkonce.s.*)
  }

  /*--------------------------------------------------------------------*/
  /* Uninitialized data segment                                         */
  /*--------------------------------------------------------------------*/

  /* Start of uninitialized data segment */
  . = ALIGN(8);
  _fbss = .;

  /* Writable uninitialized small data segment */
  .sbss : 
  {
    *(.sbss)
    *(.sbss.*)
    *(.gnu.linkonce.sb.*)
  }

  /* bss: Uninitialized writeable data section */
  . = .;
  _bss_start = .;
  .bss : 
  {
    *(.bss)
    *(.bss.*)
    *(.gnu.linkonce.b.*)
    *(COMMON)
  }

  /* End of uninitialized data segment (used by syscalls.c for heap) */
  PROVIDE( end = . );
  _end = ALIGN(8);
}
