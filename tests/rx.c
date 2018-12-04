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
#define BITS_WIDTH 24
#define BITS_MASK ((1L << BITS_WIDTH) - 1)
#define PKT_START_MASK (1L << BITS_WIDTH)
#define PKT_END_MASK (1L << (BITS_WIDTH + 1))
#define IS_HEAD_MASK (1L << (BITS_WIDTH + 2))
#define PUNCTURE_WIDTH 4
#define PUNCTURE_MASK (((1L << PUNCTURE_WIDTH) - 1) << (BITS_WIDTH + 3))
#define MODULATION_MASK (3L << (BITS_WIDTH + PUNCTURE_WIDTH + 3))


/**
 * Pack modem fields into 64-bit unsigned integer.
 * Make sure the #defines above are correct!
 * You will need to pack into multiple uint64_t if 2 * XY_WIDTH + Z_WIDTH + 1 > 64
 */
uint64_t pack_modem_tx(int64_t bits, uint8_t pktStart, uint8_t pktEnd,
                    uint8_t isHead, uint8_t puncture, uint8_t modulation) {
  return bits | (pktStart & 0x1) << BITS_WIDTH |
         (pktEnd & 0x1) << (BITS_WIDTH + 1) |
         (isHead & 0x1) << (BITS_WIDTH + 2) |
         (puncture & 0xf) << (BITS_WIDTH + 3) |
         (modulation & 0x3) << (BITS_WIDTH + PUNCTURE_WIDTH + 3);
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
  uint64_t packSignal = pack_modem_tx(0xd30080, 1, 0, 1, 0xd, 0);
  uint64_t packService = pack_modem_tx(0xf0f0f0, 0, 0, 0, 0xd, 0);
  uint64_t packData = pack_modem_tx(data, 0, 1, 0, 0xd, 0);

  // Write data
  while(reg_read8(RX_WRITE_COUNT) > 4) {
    printf("Waiting for cordic queue to empty...\n");
  }
  reg_write64(RX_WRITE, packSignal);
  reg_write64(RX_WRITE, packService);
  reg_write64(RX_WRITE, packData);

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
