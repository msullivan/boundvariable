/* UM.C with network */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>


#include <sys/poll.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <string.h>
#include <malloc.h>

typedef unsigned int uint;

#if 0
int ml_accept(int l) {
  struct sockaddr remote;
  int sin_size = sizeof (struct sockaddr_in);
  int e = 0;
  sigset_t blockme;


  if (sigfillset(&blockme))
    return -1;
  if (sigprocmask(SIG_BLOCK, &blockme, NULL))
    return -1;
  if ((e = accept(l, (struct sockaddr*)&remote, &sin_size)) == -1)
    printf("ACCEPT ERROR: %s\n", strerror(errno));
  if (sigprocmask(SIG_UNBLOCK, &blockme, NULL))
    return -1;

  fcntl(e, F_SETFD, 1); /* don't keep open across exec */

  return e;
}
#endif

int bufsize = 0;
#define MAXBUF 128
char buffer[MAXBUF + 1];

void sendall(int sock, char * b, int n) {
  int r;
  if (n == 0) return;
  r = send(sock, b, n, 0);
  if (r < 0) {
      fprintf(stderr, "send error\n");
      exit(-1);
  } else {
    sendall(sock, b + r, n - r);
  }
}

void flushbuffer(int sock) {
  if (bufsize) {
    buffer[bufsize] = 0;
    sendall(sock, buffer, bufsize + 1);
    bufsize = 0;
  }
}

/* these expect packets to be terminated by 0
   because flash talks "XML" protocol */
int recvchar(int sock) {
  char ch = 0;
  flushbuffer(sock);
  while (!ch) {
    if (1 != recv(sock, &ch, 1, 0)) {
      fprintf(stderr, "EOF on socket??\n");
      exit(-1);
    }
  }
  return ch;
}


#ifdef NOBUF

void sendchar(int sock, int c) {
  char cc[2];
  cc[0] = c; cc[1] = 0;
  putchar(c);
  if (2 != send(sock, &cc, 2, 0)) {
    fprintf(stderr, "couldn't send??\n");
    exit(-1);
  }
}

#else


void sendchar(int sock, int c) {
  if (bufsize == MAXBUF) {
    flushbuffer(sock);
  }

  buffer[bufsize++] = c;

  if (c <= 32) flushbuffer (sock);
  
  /*
  char cc[2];
  cc[0] = c; cc[1] = 0;
  putchar(c);
  if (2 != send(socket, &cc, 2, 0)) {
    fprintf(stderr, "couldn't send??\n");
    exit(-1);
  }
  */
}
#endif

static uint * ulloc(uint size) {
  uint * r = (uint*)calloc((1 + size), sizeof(uint));
  *r = size;
  return (r + 1);
}

static void ufree(uint * p) {
  free(p - 1);
}

int main (int argc, char ** argv) {
  uint * zero; uint ip = 0; uint reg[8] = {0,0,0,0,0,0,0,0};

  char filename[512];
  int portnum;
  #define FILENAME "c:\\code\\homework7\\langs\\umix\\umix.um"
  #define PORTNUM 2006

  if (argc < 2) {
    sprintf(filename, "%s", FILENAME);
  } else {
    sprintf(filename, "%s", argv[1]);
  }

  if (argc < 3) {
    portnum = PORTNUM;
  } else {
    portnum = atoi(argv[2]);
  }

  FILE * f = fopen(filename, "rb");
  if (!f) {
    fprintf (stderr, "can't open %s\n", filename);
    return -1;
  }

  {
    /* assume if we opened it, we can stat it */
    struct stat buf;
    if (stat(filename, &buf)) return -1;
    else zero = ulloc(buf.st_size >> 2);
  }

  /* initialize */
  { 
    int a;
    int n = 4;
    int i = 0;
    while(EOF != (a = fgetc(f))) {
      if (!n--) {
	// test bad endianness:
	// zero[i] = ntohl(zero[i]);
	i++; n = 3; }
      zero[i] <<= 8;
      zero[i]  |= a;
    }
  }

  fprintf(stderr, "loaded.\n");

  int listener = socket(PF_INET, SOCK_STREAM, 0);
  {
    struct sockaddr_in addr;
    
    addr.sin_family = AF_INET;
    addr.sin_port = htons(portnum);
    addr.sin_addr.s_addr = 0; /* INADDR_ANY */
    memset(&(addr.sin_zero), '\0', 8);
    
    if (bind(listener, (struct sockaddr *)&addr, sizeof(struct sockaddr))) {
      fprintf(stderr, "BIND FAILED.\n");
      exit(-1);
    }
    
    if (listen(listener, 20)) {
      fprintf(stderr, "LISTEN FAILED.\n");
      exit(-1);
    }
  }

  int peer = 0;
  for(;;) {
    struct sockaddr remote;
    int sin_size = sizeof (struct sockaddr_in);
    sigset_t blockme;
    
    
    //if (sigfillset(&blockme))
    //return -1;
    //if (sigprocmask(SIG_BLOCK, &blockme, NULL))
    //return -1;
    fprintf(stderr, "waiting for connections...\n");
    if ((peer = accept(listener, (struct sockaddr*)&remote, &sin_size)) == -1)
      fprintf(stderr, "ACCEPT ERROR: %s\n", strerror(errno));

    /* child process continues... */
    if (!fork ()) break;
  }

#  define arr(m) (m?(uint*)m:zero)

#  define c w & 7
#  define b (w >> 3) & 7
#  define a (w >> 6) & 7

  /* spin cycle */
  for(;;) {
    uint w = zero[ip++];

    /*
    int c = w & 7;
    int b = (w >> 3) & 7;
    int a = (w >> 6) & 7;
    */

    switch(w >> 28) {
    case 0: if (reg[c]) reg[a] = reg[b]; break;
    case 1: reg[a] = arr(reg[b])[reg[c]]; break;
    case 2: arr(reg[a])[reg[b]] = reg[c]; break;
    case 3: reg[a] = reg[b] + reg[c]; break; 
    case 4: reg[a] = reg[b] * reg[c]; break;
    case 5: reg[a] = reg[b] / reg[c]; break;
    case 6: reg[a] = ~(reg[b] & reg[c]); break;
    case 7: flushbuffer (peer); return 0;
    case 8: reg[b] = (uint)ulloc(reg[c]); break;
    case 9: ufree((uint*)reg[c]); break;
    case 10: sendchar(peer, reg[c]); break;
    case 11: reg[c] = recvchar(peer); break;
    case 12:
      if (reg[b]) {
	ufree(zero);
	int size = ((uint*)reg[b])[-1];
	zero = ulloc(size);
	memcpy(zero, (uint*)reg[b], size * 4);
      }
      ip = reg[c]; 
      break;
    case 13: reg[7 & (w >> 25)] = w & 0177777777; break;
    }
  }
}
