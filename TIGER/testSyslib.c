//
// testlib -- test the system library
//

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

// Tiger types
typedef char*	STRING;
typedef int32_t	INT;
typedef int32_t BOOL;
typedef void    UNIT;
typedef int32_t _TPTR;

// Tiger values
const BOOL TRUE  = 1;
const BOOL FALSE = 0;

// Tiger system-library routines
extern UNIT	_print(STRING);
extern UNIT	_flush();
extern BOOL	_strComp(STRING, STRING);
extern INT	_size(STRING);
extern STRING	_concat(STRING, STRING);
extern STRING	_substring(STRING, INT, INT);
extern UNIT	_exit(INT);
extern BOOL	_not(BOOL);
extern INT	_ord(STRING);
extern STRING	_chr(INT);
extern STRING	_getchar();

// Tiger system-library support routines (for my compiler)
extern _TPTR	_allocBytes(INT);
extern _TPTR	_allocWords(INT);
extern UNIT	_nilPtr();
extern UNIT	_indexOutOfBounds();

int main() {
  STRING s1 = "this should print first";
  STRING s2 = "this should print second";
  _print(s1);
  _flush();
  _print(s2);
  _flush();

  assert(0 == _strComp(s1, s2));
  assert(1 == _strComp(s1, s1));
  assert(1 == _strComp(s2, s2));
  assert(0 == _strComp(s1, ""));

  INT s1l = strlen(s1);
  INT s2l = strlen(s2);
  assert(s1l == _size(s1));
  assert(s2l == _size(s2));
  assert(0 == _size(""));

  // tests allocations and heap also ...
  STRING s3 = _concat(s1, s2);
  _print(s3);
  _flush();
  assert(s1l+s2l == _size(s3));
  assert(s1l == _size(s1));
  assert(s2l == _size(s2));

  const INT L = 12;
  STRING s4 = _substring(s1, 5, L);
  assert(s1l+s2l == _size(s3));
  assert(L == _size(s4));
  _print(s4);
  _flush();
  STRING s5 = _substring(s1, 0, 100);
  assert(s1l == _size(s5));
  STRING s6 = _substring(s1, 1, 0);
  assert(0 == _size(s6));
  assert(s1l == _size(s1));
  
  assert(TRUE  == _not(FALSE));
  assert(FALSE == _not(TRUE));
  assert(TRUE  == _not(_not(TRUE)));

  assert(((INT) 't') == _ord(s1));
  assert(((INT) 's') == _ord(s4));
  assert(TRUE == _strComp("t", _chr((INT) 't')));
  assert(TRUE == _strComp(" ", _chr((INT) ' ')));

  printf("Press '.' and enter\n");
  assert(TRUE == _strComp(".", _getchar()));

  return EXIT_SUCCESS;
}
