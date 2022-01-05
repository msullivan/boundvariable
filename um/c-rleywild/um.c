#include "common.h"
#include "array.h"

void
state_reg ()
{
  index_t i;
  for (i = 0; i < 0; i ++)
  {
    dprintf ("r[%d] = %8.8x\n", i, reg[i]);
  }
}

void
state_finger ()
{
  dprintf ("platter = %d, offset = %d\n", platter, offset);
}

void
state_op (platter_t op)
{
  dprintf ("op = %1.1x, A = %1.1x, B = %1.1x, C = %1.1x, A2 = %1.1x, IMM = %7.7x\n", SUB_OP (op), SUB_A (op), SUB_B (op), SUB_C (op), SUB_A2 (op), SUB_IMM (op));
}

void
exec (char * file)
{
  /* initialize registers */
  index_t r;
  for (r = 0; r < 8; r ++)
  {
    reg[r] = 0;
  }

  /* initialize finger */
  platter = 0;
  offset = 0;

  /* load memory */
  FILE * fp;
  size_t sz;
  index_t b, i;
  platter_t buf[BUFSIZ];
  assert ((fp = fopen (file, "r")) != NULL);
  assert (fseek (fp, 0L, SEEK_END) != -1);
  sz = ftell (fp);
  assert (array_alloc (sz) == 0);
  assert (fseek (fp, 0L, SEEK_SET) == 0);
  for (b = 0; ! feof (fp); b += BUFSIZ)
  {
    sz = fread (buf, sizeof (platter_t), BUFSIZ, fp);
    for (i = 0; i < sz; i ++)
    {
      array_set (platter, b + i, bswap_32 (buf[i]));
    }
  }
  assert (fclose (fp) == 0);

  /* execute */
  while (1)
  {
    platter_t op;
    op = array_get (platter, offset);
/*
state_finger ();
state_reg ();
state_finger ();
state_op (op);
*/
    switch (SUB_OP (op))
    {
    case 0:
      dprintf ("OP:0: r[C] == 0 || r[A] = r[B]\n");
      (reg[SUB_C (op)] == 0) || (reg[SUB_A (op)] = reg[SUB_B (op)]);
      offset ++;
      break;
    case 1:
      dprintf ("OP:1: r[A] = ARRAY[r[B], r[C]]\n");
      reg[SUB_A (op)] = array_get (reg[SUB_B (op)], reg[SUB_C (op)]);
      offset ++;
      break;
    case 2:
      dprintf ("OP:2: ARRAY[r[A], r[B]] = r[C]\n");
      array_set (reg[SUB_A (op)], reg[SUB_B (op)], reg[SUB_C (op)]);
      offset ++;
      break;
    case 3:
      dprintf ("OP:3: r[A] = r[B] + r[C]\n");
      reg[SUB_A (op)] = reg[SUB_B (op)] + reg[SUB_C (op)];
      offset ++;
      break;
    case 4:
      dprintf ("OP:4: r[A] = r[B] * r[C]\n");
      reg[SUB_A (op)] = reg[SUB_B (op)] * reg[SUB_C (op)];
      offset ++;
      break;
    case 5:
      dprintf ("OP:5: r[A] = r[B] / r[C]\n");
      assert (reg[SUB_C (op)] != 0);
      reg[SUB_A (op)] = reg[SUB_B (op)] / reg[SUB_C (op)];
      offset ++;
      break;
    case 6:
      dprintf ("OP:6: r[A] = ~(r[B] & r[C])\n");
      reg[SUB_A (op)] = ~(reg[SUB_B (op)] & reg[SUB_C (op)]);
      offset ++;
      break;
    case 7:
      dprintf ("OP:7: HALT\n");
      dprintf ("halt at platter = %d, offset = %d\n", platter, offset);
      exit (0);
    case 8:
      dprintf ("OP:8: r[B] = new(r[C])\n");
      reg[SUB_B (op)] = array_alloc (reg[SUB_C (op)]);
      offset ++;
      break;
    case 9:
      dprintf ("OP:9: free(r[C])\n");
      array_free (reg[SUB_C (op)]);
      offset ++;
      break;
    case 10:
      dprintf ("OP:A: out = r[C]\n");
      assert (reg[SUB_C (op)] < 256);
      con = reg[SUB_C (op)];
      printf ("%c", con);
      dprintf ("con = %c\n", con);
      offset ++;
      break;
    case 11:
      dprintf ("OP:B: r[C] = in\n");
      /* XXX console input */
      {
        int c;
        c = getc (stdin);
        reg[SUB_C (op)] = (c == EOF) ? 0xffffffff : c;
      }
      /*
      if (feof (stdin))
      {
        printf ("done\n");
        reg[SUB_C (op)] = 0xffffffff;
      }
      else
      {
        console_t c;
        assert (fread (&c, sizeof (char), 1, stdin) == 1);
        dprintf ("read = %c\n", c);
        reg[SUB_C (op)] = c;
      }
      */
      offset ++;
      break;
    case 12:
      dprintf ("OP:C: ARRAY[0] = ARRAY[r[B]]; finger = r[C]\n");
      array_copy (reg[SUB_B (op)]);
      platter = 0;
      offset = reg[SUB_C (op)];
      break;
    case 13:
      dprintf ("OP:D: r[A2] = IMM\n");
      reg[SUB_A2 (op)] = SUB_IMM (op);
      offset ++;
      break;
    default:
      dprintf ("INVALID\n");
      assert (0);
    }
  }

}

int
main (int argc, char ** argv)
{
  array_init ();
  assert (argc == 2);
  exec (argv[1]);
  return 0;
}
