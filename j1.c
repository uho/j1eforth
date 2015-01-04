#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(unix) || defined(__unix__) || defined(__unix) || (defined(__APPLE__) && defined(__MACH__))
#include <unistd.h>
#include <termios.h>
int getch(void) { /* reads from keypress, doesn't echo */
    struct termios oldattr, newattr;
    int ch;
    tcgetattr( STDIN_FILENO, &oldattr );
    newattr = oldattr;
    newattr.c_iflag &= ~( ICRNL );
    newattr.c_lflag &= ~( ICANON | ECHO );
    tcsetattr( STDIN_FILENO, TCSANOW, &newattr );
    ch = getchar();
    tcsetattr( STDIN_FILENO, TCSANOW, &oldattr );
    // printf("%d\n", ch);
    return ch==127 ? 8 : ch;
}

int putch(int c) { /* output character to sstdout & flush */
    int res=putchar(c);
    fflush(stdout);
    return res;
}
#endif

static unsigned short t;  
static unsigned short s;
static unsigned short d[0x20]; /* data stack */
static unsigned short r[0x20]; /* return stack */
static unsigned short pc;    /* program counter, counts cells */
static unsigned char dsp, rsp; /* point to top entry */
static unsigned short* memory; /* ram */
static int sx[4] = { 0, 1, -2, -1 }; /* 2-bit sign extension */

static void push(int v) // push v on the data stack
{
  dsp = 0x1f & (dsp + 1);
  d[dsp] = t;
  t = v;
}

static int pop(void) // pop value from the data stack and return it
{
  int v = t;
  t = d[dsp];
  dsp = 0x1f & (dsp - 1);
  return v;
}

static void execute(int entrypoint)
{
  int _pc, _t;
  int insn = 0x4000 | entrypoint; // first insn: "call entrypoint"
  do {
    // printf("pc=%04X, insn=%04X, dsp=%d, s=%04X, t=%04X\n", pc, insn, dsp, s, t);
    _pc = pc + 1;
    if (insn & 0x8000) { // literal
      push(insn & 0x7fff);
    } else {
      int target = insn & 0x1fff;
      switch (insn >> 13) {
      case 0: // jump
        _pc = target;
        break;
      case 1: // conditional jump
		if (pop() == 0)
          _pc = target;
        break;
      case 2: // call
        rsp = 31 & (rsp + 1);
        r[rsp] = _pc << 1;
        _pc = target;
        break;
      case 3: // alu
		if (insn & 0x1000) {/* r->pc */
			_pc = r[rsp] >> 1;
		}
        s = d[dsp];
        switch ((insn >> 8) & 0xf) {
        case 0:   _t = t; break; /* noop */
        case 1:   _t = s; break; /* copy */
        case 2:   _t = t+s; break; /* + */
        case 3:   _t = t&s; break; /* and */
        case 4:   _t = t|s; break; /* or */
        case 5:   _t = t^s; break; /* xor */
        case 6:   _t = ~t; break; /* invert */
        case 7:   _t = -(t==s); break; /* = */
        case 8:   _t = -((signed short)s < (signed short)t); break; /* < */
        case 9:   _t = s>>t; break; /* rshift */
        case 0xa:  _t = t-1; break; /* 1- */
        case 0xb:  _t = r[rsp];  break; /* r@ */
        case 0xc:  _t = (t==0xf001)?1:(t==0xf000)?getch():memory[t>>1];if(_t==0x1b)exit(0); break; /* @ */
        case 0xd:  _t = s<<t; break; /* lshift */
        case 0xe:  _t = (rsp<<8) + dsp; break; /* dsp */
        case 0xf:  _t = -(s<t); break; /* u< */
        }
        dsp = 31 & (dsp + sx[insn & 3]); /* dstack+- */
        rsp = 31 & (rsp + sx[(insn >> 2) & 3]); /* rstack+- */
        if (insn & 0x80) /* t->s */ 
           d[dsp] = t;
        if (insn & 0x40) /* t->r */
           r[rsp] = t;
        if (insn & 0x20) /* s->[t] */
          (t==0xf002)?(rsp=0):(t==0xf000)?putch(s):(memory[t>>1]=s); /* ! */
		t = _t;
        break;
      }
    }
    pc = _pc;
    insn = memory[pc];
  } while (1);
}
/* end of cpu */

/* start of i/o demo */


int main(int argc , char *argv[]) 
{
  unsigned short m[0x8000];
  memset((void*)m, 0, sizeof(m));
  FILE *f = fopen("j1.bin", "r");
  fread(m, 0x2000, sizeof(m[0]), f);
  fclose(f);
  if (argc>1) {  // program name is counted as one
   f = fopen(argv[1], "r");
   fread(&m[0x2000], 0x2000, sizeof(m[0]), f);
   fclose(f);
  }
  memory = m;
  execute(0x00);
  return 0;
}
