#define CORDIC_WRITE 0x2000
#define CORDIC_WRITE_COUNT 0x2008
#define CORDIC_READ 0x2100
#define CORDIC_READ_COUNT 0x2108

#include <stdio.h>
// #include <math.h>

#include "mmio.h"

/**
 * Make sure these #defines are correct for your chosen parameters.
 * You'll get really strange (and wrong) results if they do not match.
 */
#define XY_WIDTH 16
#define XY_BP (XY_WIDTH - 3)
#define XY_SCALE 8192
#define XY_MASK ((1L << XY_WIDTH) - 1)
#define Z_WIDTH 16
#define Z_BP (Z_WIDTH - 2)
#define Z_SCALE 16384
#define Z_MASK ((1L << Z_WIDTH) - 1)

/**
 * Pack cordic fields into 64-bit unsigned integer.
 * Make sure the #defines above are correct!
 * You will need to pack into multiple uint64_t if 2 * XY_WIDTH + Z_WIDTH + 1 > 64
 */
uint64_t pack_cordic(double x, double y, double z, uint8_t vectoring) {
  int64_t xint = (int64_t)(x * (1L << XY_BP));
  int64_t yint = (int64_t)(y * (1L << XY_BP));
  int64_t zint = (int64_t)(z * (1L << Z_BP));

  uint64_t xpack = ((uint64_t)xint) & XY_MASK;
  uint64_t ypack = ((uint64_t)yint) & XY_MASK;
  uint64_t zpack = ((uint64_t)zint) & Z_MASK;
  uint64_t vpack = vectoring ? 1 : 0;

  return
    (vpack << (2 * XY_WIDTH + Z_WIDTH)) |
    (xpack << (XY_WIDTH + Z_WIDTH))     |
    (ypack << Z_WIDTH)                  |
    zpack;
}

/*
 * Unpack output of cordic and get an integer version of x.
 * We can't printf() a double, so printing ints is the way to go.
 * To get the floating point version, divide this by 2^XY_BP
 */
int64_t unpack_cordic_x(uint64_t packed) {
  uint64_t xpack = (packed >> (XY_WIDTH + Z_WIDTH)) & XY_MASK;
  int shift = 64 - XY_WIDTH;
  return ((int64_t)(xpack << shift)) >> shift;
}

/*
 * Unpack output of cordic and get an integer version of y.
 * We can't printf() a double, so printing ints is the way to go.
 * To get the floating point version, divide this by 2^XY_BP
 */
int64_t unpack_cordic_y(uint64_t packed) {
  uint64_t ypack = (packed >> Z_WIDTH) & XY_MASK;
  int shift = 64 - XY_WIDTH;
  return ((int64_t)(ypack << shift)) >> shift;
}

/*
 * Unpack output of cordic and get an integer version of z.
 * We can't printf() a double, so printing ints is the way to go.
 * To get the floating point version, divide this by 2^Z_BP
 */
int64_t unpack_cordic_z(uint64_t packed) {
  uint64_t zpack = packed & Z_MASK;
  int shift = 64 - Z_WIDTH;
  return ((int64_t)(zpack << shift)) >> shift;
}

int expect(int64_t got, int64_t expected, int64_t tolerance){
  if((expected - got) > tolerance || (expected - got) < -tolerance)
    return 0;
  else
    return 1;
}

int main(void){
  /*printf("%d\n", 3);*/
  printf("Begin tests...\n\n");

  double x [7] = {1,1,1,1,1,1,1}; // x test vector
  double y [7] = {0,0,0,0,0,0,0}; // y test vector
  double z [7] = {-1,-0.5,-0.25,0,0.25,0.5,1}; // z test vector
  char z_val [7][5] = {"-1","-0.5","-0.25","0","0.25","0.5","1"};
  uint8_t vectoring [7] = {0,0,0,0,0,0,0}; // vectoring test vector (Rotation Only)

  double answers [7] = {0.540302305868140,0.877582561890373,0.968912421710645,1.000000000000000,0.968912421710645,0.877582561890373,0.540302305868140};
  int64_t tolerance = 3;
  int results [7];

  int64_t scaled_answer;

  uint64_t write_pack;
  uint64_t read_pack;
  int64_t cord_x, cord_y, cord_z;

  printf("Begin rotation tests (x=1,y=0)...\n");
  printf("Writing tests...");
  for(int i = 0; i < 7 ; i++){
    write_pack = pack_cordic(x[i], y[i], z[i], vectoring[i]);
    reg_write64(CORDIC_WRITE, write_pack);
  }
  printf("written.\n");
  for(int i = 0; i < 7; i++){
    /*while(reg_read64(CORDIC_READ_COUNT == 0));*/
    read_pack = reg_read64(CORDIC_READ);

    /*printf("Unpacking data...\n");*/
    cord_x = unpack_cordic_x(read_pack);
    cord_y = unpack_cordic_y(read_pack);
    cord_z = unpack_cordic_z(read_pack);

    scaled_answer = answers[i] * XY_SCALE;
    printf("Testing z=%s...", z_val[i]);

    if (expect(cord_x, scaled_answer, tolerance) == 1)
      printf("PASSED\n");
    else
      printf("FAILED\n");
  }

  printf("\nBegin vectoring tests (x=1,z=0)...\n");
  double vec_x [7] = {1,1,1,1,1,1,1};
  double vec_y [7] = {-1,-0.5,-0.25,0,0.25,0.5,1};
  double vec_z [7] = {0,0,0,0,0,0,0};
  double vectoring_2 [7] = {1,1,1,1,1,1,1};
  double answers_vec [7] = {-0.785398163397448,-0.463647609000806,-0.244978663126864,0,0.244978663126864,0.463647609000806,0.785398163397448};

  printf("Writing tests...");
  for(int i = 0; i < 7 ; i++){
    write_pack = pack_cordic(vec_x[i], vec_y[i], vec_z[i], vectoring_2[i]);
    reg_write64(CORDIC_WRITE, write_pack);
  }
  printf("written.\n");
  for(int i = 0; i < 7; i++){
    /*while(reg_read64(CORDIC_READ_COUNT == 0));*/
    read_pack = reg_read64(CORDIC_READ);

    /*printf("Unpacking data...\n");*/
    cord_x = unpack_cordic_x(read_pack);
    cord_y = unpack_cordic_y(read_pack);
    cord_z = unpack_cordic_z(read_pack);

    scaled_answer = answers_vec[i] * Z_SCALE;
    printf("Testing y=%s...", z_val[i]);

    if (expect(cord_z, scaled_answer, tolerance) == 1)
      printf("PASSED\n");
    else
      printf("FAILED\n");
  }



	return 0;
}
