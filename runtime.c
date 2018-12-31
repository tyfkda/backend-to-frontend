#include <stdio.h>
#include <stdlib.h>  // for exit
#include <sys/mman.h>
#include <unistd.h>

#ifndef FALSE
#define FALSE (0)
#define TRUE  (1)
#endif

/* define all scheme constatns */
#define	tag_mask    0x07
#define	other_tag   0x07
#define	bool_f      0x2f
#define	bool_t      0x6f
#define	fx_mask     0x03
#define	fx_tag      0x00
#define	fx_shift    2
#define	nullval     0x3f
#define	char_mask   0xff
#define	char_shift  8
#define	char_tag    0x0f
#define	pair_tag    0x01
#define	vector_tag  0x05
#define	string_tag  0x06

/* all scheme values are of type ptrs */
typedef unsigned int ptr;

typedef struct {
  void* eax;  /* 0   scratch  */
  void* ebx;  /* 4   preserve */
  void* ecx;  /* 8   scratch  */
  void* edx;  /* 12  scratch  */
  void* esi;  /* 16  preserve */
  void* edi;  /* 20  preserve */
  void* ebp;  /* 24  preserve */
  void* esp;  /* 28  preserve */
} context;

extern int scheme_entry(context* ctxt, void* stack, void* heap);


typedef struct {
  ptr car;
  ptr cdr;
} cell;

typedef struct {
  unsigned int num;
  ptr buf[1];  // num
} vector;

typedef struct {
  unsigned int num;
  char buf[1];  // num
} string;

static inline ptr CAR(ptr x) { return ((cell*)(x-pair_tag))->car; }
static inline ptr CDR(ptr x) { return ((cell*)(x-pair_tag))->cdr; }
static inline int consp(ptr x) { return (x & tag_mask) == pair_tag; }
static inline int nullp(ptr x) { return x == nullval; }

static void print_ptr_sub(ptr x) {
  switch (x & tag_mask) {
  default:
    break;
  case fx_tag: case fx_tag + 4:
    printf("%d", ((int)x) >> fx_shift);
    return;
  case other_tag:
    if ((x & char_mask) == char_tag) {
      int c = x >> char_shift;
      switch (c) {
      default:	  printf("#\\%c", c);  break;
      case '\t':  printf("#\\tab");  break;
      case '\n':  printf("#\\newline");  break;
      case '\r':  printf("#\\return");  break;
      case ' ':	  printf("#\\space");  break;
      }
      return;
    } else {
      switch (x) {
      default:  break;
      case bool_f:   printf("#f");  return;
      case bool_t:   printf("#t");  return;
      case nullval:  printf("()");  return;
      }
    }
  case pair_tag:
    {
      int first = TRUE;
      printf("(");
      for (; consp(x); x = CDR(x)) {
        if (!first) printf(" ");
        print_ptr_sub(CAR(x));
        first = FALSE;
      }
      if (!nullp(x)) {
        printf(" . ");
        print_ptr_sub(x);
      }
      printf(")");
    }
    return;
  case vector_tag:
    {
      vector* p = (vector*)(x - vector_tag);
      int first = TRUE;
      unsigned int n = p->num >> fx_shift;
      unsigned int i;
      printf("#(");
      for (i=0; i<n; ++i) {
        if (!first) printf(" ");
        first = FALSE;
        print_ptr_sub(p->buf[i]);
      }
      printf(")");
    }
    return;
  case string_tag:
    {
      string* p = (string*)(x - string_tag);
      unsigned int n = p->num;
      unsigned int i;
      printf("\"");
      for (i=0; i<n; ++i) {
        int c = p->buf[i];
        switch (c) {
        default:   putchar(c);  break;
        case '"':  fputs("\\\"", stdout);  break;
        case '\\': fputs("\\\\", stdout);  break;
        }
      }
      printf("\"");
    }
    return;
  }

  printf("#<unknown #0x%08x>", x);
}

static void print_ptr(ptr x) {
  print_ptr_sub(x);
  printf("\n");
}

static char* allocate_protected_space(int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  char* p = mmap(0, aligned_size + 2 * page, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, 0, 0);
  if (p == MAP_FAILED) { exit(1); }
  status = mprotect(p, page, PROT_NONE);
  if (status != 0) { exit(1); }
  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if (status != 0) { exit(1); }
  return p + page;
}

static void deallocate_protected_space(char* p, int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  status = munmap(p - page, aligned_size + 2 * page);
  if (status != 0) { exit(1); }
}

int main(int argc, char** argv) {
  int stack_size = (16 * 4096);	/* holds 16K cells */
  char* stack_top = allocate_protected_space(stack_size);
  char* stack_base = stack_top + stack_size;
  int heap_size = (16 * 4096);	/* holds 16K cells */
  char* heap = allocate_protected_space(heap_size);
  context ctxt;
  print_ptr(scheme_entry(&ctxt, stack_base, heap));
  deallocate_protected_space(heap, heap_size);
  deallocate_protected_space(stack_top, stack_size);
  return 0;
}
