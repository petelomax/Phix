// compiled with: tcc -I . -L . blc.c
#include <stdio.h>

void Error(int ec, char*s) {
    printf(s);
    exit(ec);
}

void Errorx(int ec, char*s, int v) {
    printf(s, v);
    exit(ec);
}

#define dbg 1
#define TERMS 50000
#define IOP 0  // code for gro, wr0, wr1, put
#define VAR 1  // code for variable lookup
#define APP 2  // code for applications
#define ABS 3  // code for abstractions
#define REF(c) (++(c)->refs, c)

//struct Parse {
//  int n;
//  int i;
//};

struct Closure {
  struct Closure *next;
  struct Closure *envp;
  int refs;
  int term;
};

static const char kRom[] = {
    APP, 0,  //  0 (» 0 » 0 (» 0 wr0 wr1) put) (main gro)
    ABS,     //  2 » 0 » 0 (» 0 wr0 wr1) put
    APP, 0,  //  3
    VAR, 0,  //  5
    ABS,     //  7
    APP,     //  8
    ABS,     //  9 » 0 » 0 wr0 wr1
    APP, 2,  // 10
    VAR,     // 12
    IOP,     // 13
    ABS,     // 14 » 0 wr0 wr1
    APP, 4,  // 15
    APP, 1,  // 17
    VAR,     // 19
    IOP,     // 20 wr0
    IOP, 0,  // 21 wr1
};

long ip;                // instruction pointer
long ecp;               // end of code pointer
int mem[TERMS];         // bss memory for terms
struct Closure *frep;   // freed closures list
struct Closure *contp;  // continuations stack
struct Closure root = {.refs = 1};
struct Closure *envp = &root;

void Gc(struct Closure *p) {
  for (; p && p != &root; p = p->envp) {
    if (--p->refs) break;
    Gc(p->next);
if (dbg) printf("free()\n");
    p->next = frep;
    frep = p;
  }
}

void Var(void) {
  int i, x;
  struct Closure *t, *e;
  e = t = envp;
  x = mem[ip + 1];
  for (i = 0; i < x && e != &root; ++i) {
if (dbg) printf("vloop: %d\n",e->term); 
    e = e->next;
  }
  if (e == &root) Errorx(10 + x, "UNDEFINED VARIABLE %d", x);
if (dbg) printf("Var: x=%d, ip:%d==>%d\n", x,ip,e->term);
//  if (e == &root) exit(998);
  ip = e->term;
  envp = REF(e->envp);
  Gc(t);
}

void Gro(void) {
  int c = fgetc(stdin), sc = ecp;
if (dbg) printf("Gro: c:%d\n",c);
//  if (c != -1) {
  if (c != -1 &&c != 26) {
    mem[ecp++] = ABS;
    mem[ecp++] = APP;
    mem[ecp++] = 8;
    mem[ecp++] = APP;
    mem[ecp++] = 2;
    mem[ecp++] = VAR;
    mem[ecp++] = 0;
    mem[ecp++] = ABS;
    mem[ecp++] = ABS;
    mem[ecp++] = VAR;
    mem[ecp++] = ~c & 1;
  } else {
    mem[ecp++] = ABS;
    mem[ecp++] = ABS;
    mem[ecp++] = VAR;
    mem[ecp++] = 0;
  }
if (dbg) printf("Gro: mem[%d..%d] (%d)\n",sc+1,ecp,mem[ecp-1]);
}

void Put(void) {
  fputc('0' + (ip & 1), stdout);
  ip = 2;
}

void Bye(void) {
  int rc = mem[ip + 2];  // (lambda 0) [exitcode]
  if (rc) Error(rc, "CONTINUATIONS EXHAUSTED");
//  if (rc) exit(997);
//  if (postdump && !rc) Dump(0, ecp, stderr);
  exit(0);
}

// pops continuation and pushes it to environment
void Abs(void) {
  if (!contp) Bye();
  struct Closure *t = contp;
  contp = t->next;
  t->next = envp;
  envp = t;
  ++ip;
}

struct Closure *Alloc(void) {
  struct Closure *t;
  if (!(t = frep)) {
    if (!(t = (struct Closure *)calloc(1, sizeof(struct Closure)))) {
      Error(6, "OUT OF HEAP");
//    exit(999);
    }
  }
  frep = t->next;
//PL ^erm, shouldn't t->next become 0 here??
  t->refs = 1;
//  ++heap;
  return t;
}

// pushes continuation for argument
void App(void) {
  int x = mem[ip + 1];
  struct Closure *t = Alloc();
  t->term = ip + 2 + x;
if (dbg) printf("App: t:??, ip:%d, x:%d, term:%d\n",ip,x,t->term);
  t->envp = t->term > 21 && t->term != ecp ? REF(envp) : &root;
  t->next = contp;
  contp = t;
  ip += 2;
}

void Iop(void) {
  if (ip == ecp) {
    Gro();
  } else {
    Put();  // ip is an element of {6,13,20,21}
  }
  Gc(envp);
  envp = &root;
}

static void Rex(void) {
  int mip = mem[ip];
if (dbg) printf("Rex: ip=%d, %d\n", ip, mip);
//  switch (mem[ip]) {
  switch (mip) {
    case VAR:
      Var();
      break;
    case APP:
      App();
      break;
    case ABS:
      Abs();
      break;
    case IOP:
      Iop();
      break;
    default:
      Error(7, "CORRUPT TERM");
//    exit(996);
  }
}

char GetBit(FILE* f) {
  int c;
  if ((c = fgetc(f)) != -1) c &= 1;
  return c;
}

char NeedBit(FILE* f) {
  char b = GetBit(f);
  if (b == -1) Error(9, "UNEXPECTED EOF");
//  if (b == -1) exit(995);
  return b;
}

//struct Parse Parse(int ignored, FILE* f) {
int Parse(int ignored, FILE* f) {
  int t, start, p;
  char bit, need;
//  struct Parse p;
  for (need = 0, start = ecp;;) {
    if (ecp + 2 > TERMS) Error(5, "OUT OF TERMS");
//  if (ecp + 2 > TERMS) exit(994);
    if ((bit = GetBit(f)) == -1) {
      if (!need) break;
      Error(9, "UNFINISHED EXPRESSION");
//    exit(993);
    } else if (bit) {
      for (t = 0; NeedBit(f);) ++t;
      mem[ecp++] = VAR;
      mem[ecp++] = t;
if (dbg) printf("mem[%d..%d] = VAR,%d\n",ecp-1,ecp,t);
      break;
    } else if (NeedBit(f)) {
      t = ecp;
      ecp += 2;
      mem[t] = APP;
//    p = Parse(0, f);
//    mem[t + 1] = p.n;
      p = Parse(0, f);
      mem[t + 1] = p;
//if (dbg) printf("mem[%d..%d] = APP,%d\n",t,t+1,p);
if (dbg) printf("mem[%d..%d] = APP,%d\n",t+1,t+2,p);
      need = 1;
    } else {
      mem[ecp++] = ABS;
if (dbg) printf("mem[%d] = ABS\n",ecp);
    }
  }
//  p.i = start;
//  p.n = ecp - start;
//  return p;
  return ecp-start;
}

void LoadRom(void) {
  long i;
  for (; ecp < sizeof(kRom) / sizeof(*kRom); ++ecp) {
    mem[ecp] = kRom[ecp];
  }
  mem[4] = 9;
  mem[1] = ecp - 2;
}

void Krivine(void) {
  int main;
  long gotoget;
  LoadRom();
  mem[ecp++] = APP;
  gotoget = ecp++;
  main = ecp;
//  mem[gotoget] = Parse(1, stdin).n;
  mem[gotoget] = Parse(1, stdin);
  for (;;) Rex();
}

int main(int argc, char **argv) 
{
    Krivine();
}
