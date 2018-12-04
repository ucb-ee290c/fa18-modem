#define RX_WRITE 0x2000
#define RX_WRITE_COUNT 0x2008
#define RX_READ 0x2100
#define RX_READ_COUNT 0x2108
//#define TX_WRITE 0x2000
//#define TX_WRITE_COUNT 0x2008
//#define TX_READ 0x2100
//#define TX_READ_COUNT 0x2108

#include <stdio.h>

#include "mmio.h"

/**
 * Make sure these #defines are correct for your chosen parameters.
 * You'll get really strange (and wrong) results if they do not match.
 */
#define BITS_WIDTH 8
#define BITS_MASK ((1L << BITS_WIDTH) - 1)
#define PKT_START_MASK (1L << BITS_WIDTH)
#define PKT_END_MASK (1L << (BITS_WIDTH + 1))
#define IS_HEAD_MASK (1L << (BITS_WIDTH + 2))
#define PUNCTURE_WIDTH 4
#define PUNCTURE_MASK (((1L << PUNCTURE_WIDTH) - 1) << (BITS_WIDTH + 3))
#define MODULATION_MASK (3L << (BITS_WIDTH + PUNCTURE_WIDTH + 3))

#define FP1 (1L << 13)
#define FPM1 (0xd000)

/**
 * Pack modem fields into 64-bit unsigned integer.
 * Make sure the #defines above are correct!
 * You will need to pack into multiple uint64_t if 2 * XY_WIDTH + Z_WIDTH + 1 > 64
 */
uint64_t pack_modem_rx(int16_t inphase, int16_t quadrature) {
  return quadrature | (inphase << 16);
}

/*
 * Unpack output of modem and get an integer version of bits.
 */
uint64_t unpack_rx_bits(uint64_t packed) {
  return packed & BITS_MASK;
}

/*
 * Unpack output of modem and get the pktStart flag.
 */
uint8_t unpack_rx_pktStart(uint64_t packed) {
  return (packed >> BITS_WIDTH) & 0x1;
}

/*
 * Unpack output of modem and get the pktEnd flag.
 */
uint8_t unpack_rx_pktEnd(uint64_t packed) {
  return (packed >> (BITS_WIDTH + 1)) & 0x1;
}

void run_modem(uint64_t data) {
  // SIGNAL field
  // rate 6Mbps, R, length 6,    parity, 0s
  // 1101        0  011000000000 1       000000
  uint64_t packPlusPlus = pack_modem_rx(FP1, FP1);
  uint64_t packPlusMinus = pack_modem_rx(FP1, FPM1);
  uint64_t packMinusPlus = pack_modem_rx(FPM1, FP1);
  uint64_t packMinusMinus = pack_modem_rx(FPM1, FPM1);

  // Write data
  int i;
  // Write STF
  for (i=0; i < 80; ++i) {
    while(reg_read8(RX_WRITE_COUNT) > 6) {
      printf("Waiting for modem queue to empty...\n");
    }
    reg_write64(RX_WRITE, packPlusPlus);
    reg_write64(RX_WRITE, packMinusMinus);
  }

  // Write LTF
  for (i=0; i < 80; ++i) {
    while(reg_read8(RX_WRITE_COUNT) > 6) {
      printf("Waiting for modem queue to empty...\n");
    }
    reg_write64(RX_WRITE, packPlusMinus);
    reg_write64(RX_WRITE, packMinusPlus);
  }


  // Write SIGNAL
  for (i=0; i < 80; ++i) {
    while(reg_read8(RX_WRITE_COUNT) > 6) {
      printf("Waiting for modem queue to empty...\n");
    }
    reg_write64(RX_WRITE, packPlusPlus);
    reg_write64(RX_WRITE, packMinusPlus);
  }


  // Write SERVICE
  for (i=0; i < 80; ++i) {
    while(reg_read8(RX_WRITE_COUNT) > 6) {
      printf("Waiting for modem queue to empty...\n");
    }
    reg_write64(RX_WRITE, packMinusMinus);
    reg_write64(RX_WRITE, packMinusPlus);
  }


  // Write DATA
  for (i=0; i < 80; ++i) {
    while(reg_read8(RX_WRITE_COUNT) > 6) {
      printf("Waiting for modem queue to empty...\n");
    }
    reg_write64(RX_WRITE, packPlusMinus);
    reg_write64(RX_WRITE, packPlusPlus);
  }

  // Read SIGNAL
  while (reg_read8(RX_READ_COUNT) == 0) {
    printf("Waiting for read queue...\n");
  }
  uint64_t result = reg_read64(RX_READ);
  printf("packet start: %d\n", unpack_rx_pktStart(result));
  printf("packet end: %d\n", unpack_rx_pktEnd(result));
  printf("packet SIGNAL: %u\n", unpack_rx_bits(result));
  // Read SERVICE
  while (reg_read8(RX_READ_COUNT) == 0) {
    printf("Waiting for read queue...\n");
  }
  result = reg_read64(RX_READ);
  printf("packet start: %d\n", unpack_rx_pktStart(result));
  printf("packet end: %d\n", unpack_rx_pktEnd(result));
  printf("packet SERVICE: %u\n", unpack_rx_bits(result));
  // Read DATA
  while (reg_read8(RX_READ_COUNT) == 0) {
    printf("Waiting for read queue...\n");
  }
  result = reg_read64(RX_READ);
  printf("packet start: %d\n", unpack_rx_pktStart(result));
  printf("packet end: %d\n", unpack_rx_pktEnd(result));
  printf("packet DATA: %u\n", unpack_rx_bits(result));
}

int main(void)
{
  int write_cnt;
  printf("starting...\n");
  // Send HI!
  uint64_t data = 0x484921;
  run_modem(data);

  // Send Cat
  data = 0x436174;
  run_modem(data);

  return 0;
}
