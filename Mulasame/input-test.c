#include <stdio.h>
#include <sys/ioctl.h>
#ifndef FIONBIO
#include <sys/fcntl.h>
#define change_para_mode ( para_flg ? fcntl(fileno(stdin), F_SETFL, O_NDELAY)\
 : fcntl(fileno(stdin), F_SETFL, O_SYNC) )
#else
#define change_para_mode ioctl(fileno(stdin), FIONBIO, &para_flg)
#endif

main () {
  int count = 0;
  int c;
  int para_flg;

  para_flg = 1;
  change_para_mode;
  printf("Hit Return Key!");
  for(;;)
    if(getchar() != EOF)
      break;
    else
      count++;

  c = count;
  para_flg = 0;
  change_para_mode;
  printf("Hit Return Key!");
  for(;;)
    if(getchar() != EOF)
      break;
    else
      count++;

  if(c != 0 && c == count)
    printf("OK!\n");
  else
    printf("Error!\n");
}
