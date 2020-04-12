#include <fst_fast.h>
#include <stdio.h>

/**
 * The starting point of the program
 * @param argc the number of arguments
 * @param argv the argument array
 */
int main(int argc, char **argv) {

  /* Success cases */

  /* { */
  /*   printf("Testing: FST a:a\n"); */
  /*   unsigned char *instrbuff = malloc(3 * 256 * sizeof(FstStateEntry)); */
  /*   create_a_to_a(instrbuff); */

  /*   unsigned short *matched_states; */
  /*   int match_success = 0; */
  /*   char *outstr = */
  /*       match_string("a", instrbuff, &match_success, &matched_states); */

  /*   printf("outstr is: \"%s\"\n", outstr); */
  /*   printf("Match success is: %d\n", match_success); */

  /*   char *outstrcurr = outstr; */
  /*   int i = 0; */
  /*   while (*outstrcurr) { */
  /*     printf("\"%c\" matched with state %d\n", *outstrcurr,
   * matched_states[i]); */
  /*     i += 1; */
  /*     outstrcurr += 1; */
  /*   } */

  /*   free(outstr); */
  /*   free(matched_states); */
  /*   free(instrbuff); */
  /*   printf("Done.\n"); */
  /* } */

  /* { */
  /*   printf("Testing: PEGREG (B/A)K against \"aab\"\n"); */
  /*   unsigned char *instrbuff = malloc(5 * 256 * sizeof(FstStateEntry)); */
  /*   create_pegreg_bak(instrbuff); */
  /*   unsigned short *matched_states; */
  /*   int match_success = 0; */
  /*   char *outstr = */
  /*       match_string("aab", instrbuff, &match_success, &matched_states); */

  /*   printf("outstr is: \"%s\"\n", outstr); */
  /*   printf("Match success is: %d\n", match_success); */

  /*   char *outstrcurr = outstr; */
  /*   int i = 0; */
  /*   while (*outstrcurr) { */
  /*     printf("\"%c\" matched with state %d\n", *outstrcurr,
   * matched_states[i]); */
  /*     i += 1; */
  /*     outstrcurr += 1; */
  /*   } */

  /*   free(outstr); */
  /*   free(matched_states); */
  /*   free(instrbuff); */
  /*   printf("Done.\n"); */
  /* } */

  /* { */
  /*   printf("Testing: PEGREG (A/B)K against \"aaab\"\n"); */
  /*   unsigned char *instrbuff = malloc(6 * 256 * sizeof(FstStateEntry)); */
  /*   create_pegreg_abk(instrbuff); */
  /*   unsigned short *matched_states; */
  /*   int match_success = 0; */
  /*   char *outstr = */
  /*       match_string("aaab", instrbuff, &match_success, &matched_states); */

  /*   printf("outstr is: \"%s\"\n", outstr); */
  /*   printf("Match success is: %d\n", match_success); */

  /*   char *outstrcurr = outstr; */
  /*   int i = 0; */
  /*   while (*outstrcurr) { */
  /*     printf("\"%c\" matched with state %d\n", *outstrcurr,
   * matched_states[i]); */
  /*     i += 1; */
  /*     outstrcurr += 1; */
  /*   } */

  /*   free(outstr); */
  /*   free(matched_states); */
  /*   free(instrbuff); */
  /*   printf("Done.\n"); */
  /* } */

  {
    printf("Testing: PEGREG (aa/bb)x against \"aax\"\n");
    InstructionTape instrtape;
    fse_initialize_tape(&instrtape);
    create_pegreg_diffmatch(&instrtape);
    unsigned short *matched_states;
    int match_success = 0;
    char *outstr = match_string("aax", instrtape.beginning, &match_success,
                                &matched_states);

    printf("outstr is: \"%s\"\n", outstr);
    printf("Match success is: %d\n", match_success);

    char *outstrcurr = outstr;
    int i = 0;
    while (*outstrcurr) {
      printf("\"%c\" matched with state %d\n", *outstrcurr, matched_states[i]);
      i += 1;
      outstrcurr += 1;
    }

    free(outstr);
    free(matched_states);
    instruction_tape_destroy(&instrtape);
    printf("Done.\n");
  }

  /* Failure cases */
  /* { */
  /*   printf("Testing: FST a:a against \"ab\"\n"); */
  /*   unsigned char *instrbuff = malloc(3 * 256 * sizeof(FstStateEntry)); */
  /*   create_a_to_a(instrbuff); */

  /*   unsigned short *matched_states; */
  /*   int match_success = 0; */
  /*   char *outstr = */
  /*       match_string("ab", instrbuff, &match_success, &matched_states); */

  /*   printf("outstr is: \"%s\"\n", outstr); */
  /*   printf("Match success is: %d\n", match_success); */

  /*   char *outstrcurr = outstr; */
  /*   int i = 0; */
  /*   while (*outstrcurr) { */
  /*     printf("\"%c\" matched with state %d\n", *outstrcurr,
   * matched_states[i]); */
  /*     i += 1; */
  /*     outstrcurr += 1; */
  /*   } */

  /*   free(outstr); */
  /*   free(matched_states); */
  /*   free(instrbuff); */
  /*   printf("Done.\n"); */
  /* } */

  /* { */
  /*   printf("Testing: PEGREG (B/A)K against \"aaab\"\n"); */
  /*   unsigned char *instrbuff = malloc(5 * 256 * sizeof(FstStateEntry)); */
  /*   create_pegreg_bak(instrbuff); */
  /*   unsigned short *matched_states; */
  /*   int match_success = 0; */
  /*   char *outstr = */
  /*       match_string("aaab", instrbuff, &match_success, &matched_states); */

  /*   printf("outstr is: \"%s\"\n", outstr); */
  /*   printf("Match success is: %d\n", match_success); */

  /*   char *outstrcurr = outstr; */
  /*   int i = 0; */
  /*   while (*outstrcurr) { */
  /*     printf("\"%c\" matched with state %d\n", *outstrcurr,
   * matched_states[i]); */
  /*     i += 1; */
  /*     outstrcurr += 1; */
  /*   } */

  /*   free(outstr); */
  /*   free(matched_states); */
  /*   free(instrbuff); */
  /*   printf("Done.\n"); */
  /* } */

  /* { */
  /*   printf("Testing: PEGREG (A/B)K against \"aab\"\n"); */
  /*   unsigned char *instrbuff = malloc(6 * 256 * sizeof(FstStateEntry)); */
  /*   create_pegreg_abk(instrbuff); */
  /*   unsigned short *matched_states; */
  /*   int match_success = 0; */
  /*   char *outstr = */
  /*       match_string("aab", instrbuff, &match_success, &matched_states); */

  /*   printf("outstr is: \"%s\"\n", outstr); */
  /*   printf("Match success is: %d\n", match_success); */

  /*   char *outstrcurr = outstr; */
  /*   int i = 0; */
  /*   while (*outstrcurr) { */
  /*     printf("\"%c\" matched with state %d\n", *outstrcurr,
   * matched_states[i]); */
  /*     i += 1; */
  /*     outstrcurr += 1; */
  /*   } */

  /*   free(outstr); */
  /*   free(matched_states); */
  /*   free(instrbuff); */
  /* } */
}
