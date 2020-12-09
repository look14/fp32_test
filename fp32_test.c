#include <stdio.h>
#include <assert.h>
#include <stdint.h>

typedef struct {
	__uint32_t S; // sign
	__uint32_t E; // exponent
	__uint32_t M; // mantissa
} f32_t;

f32_t float_to_f32(float src) {
	f32_t ret;
	__uint32_t tmp = *(__uint32_t*)(&src);
	ret.S = (tmp >> 31) & 0x1;
	ret.E = (tmp >> 23) & 0xFF;
	ret.M = (tmp >>  0) & 0x7FFFFF;
	return ret;
}

float f32_to_float(f32_t src) {
	__uint32_t tmp;
	tmp  = src.S & 0x1     ;  tmp <<= 8;
	tmp |= src.E & 0xFF    ;  tmp <<= 23;
	tmp |= src.M & 0x7FFFFF;
	return *(float*)(&tmp);
}

__uint32_t round_to_nearest(__uint32_t src, __uint32_t right_shift) {
	if(0 == right_shift)
		return src;

	__uint32_t dst = src >> right_shift;
	__uint32_t remain = src & ((1 << right_shift) - 1);
	__uint32_t bit_test = 1 << (right_shift - 1);

	if(0 == remain)
		return dst;

	if(remain > bit_test) {
		dst += 1;
	}
	else if(remain == bit_test) {
		dst += 1;
	}

	return dst;
}

int f32_check(f32_t lh, char op, f32_t rh, f32_t* ret) {
	f32_t inf  = {0, 0xFF, 0};
	f32_t inf2 = {1, 0xFF, 0};
	f32_t nan  = {0, 0xFF, 1};

	#define IS_INF(x)	( (0==x.S) && (0xFF==x.E) && (0==x.M) )
	#define IS_INF2(x)	( (1==x.S) && (0xFF==x.E) && (0==x.M) )
	#define IS_NAN(x)	( (0xFF==x.E) && (0!=x.M) )
	#define IS_NUM(x)	( (0xFF!=x.E) )
	#define IS_ZERO(x)	( (0==x.E) && (0==x.M) )

	#define GROUP(cond1, cond2, lh, rh)	( (cond1(lh) && cond2(rh)) || (cond1(rh) && cond2(lh)) )

	if(IS_NAN(lh) || IS_NAN(rh)) *ret = nan;
	else if(GROUP(IS_INF, IS_INF2, lh, rh) && '+'==op)	*ret = nan;
	else if(GROUP(IS_INF, IS_ZERO, lh, rh) && '+'==op)	*ret = inf;
	else if(GROUP(IS_INF2, IS_ZERO, lh, rh) && '+'==op)	*ret = inf2;
	else if(GROUP(IS_INF, IS_NUM, lh, rh) && '+'==op)	*ret = inf;
	else if(GROUP(IS_INF2, IS_NUM, lh, rh) && '+'==op)	*ret = inf2;

	else if(GROUP(IS_INF, IS_INF2, lh, rh) && '*'==op)	*ret = inf2;
	else if(GROUP(IS_INF, IS_ZERO, lh, rh) && '*'==op)	*ret = nan;
	else if(GROUP(IS_INF2, IS_ZERO, lh, rh) && '*'==op)	*ret = nan;
	else if(GROUP(IS_INF, IS_NUM, lh, rh) && '*'==op)	{*ret = inf; ret->S = lh.S ^ rh.S;}
	else if(GROUP(IS_INF2, IS_NUM, lh, rh) && '*'==op)	{*ret = inf2; ret->S = lh.S ^ rh.S;}
	else return 0;
	return 1;
}

int f32_compare(f32_t lh, f32_t rh) {
	#define IS_INF(x)	( (0==x.S) && (0xFF==x.E) && (0==x.M) )
	#define IS_INF2(x)	( (1==x.S) && (0xFF==x.E) && (0==x.M) )
	#define IS_NAN(x)	( (0xFF==x.E) && (0!=x.M) )

	     if(IS_INF (lh) && IS_INF (rh)) return 1;
	else if(IS_INF2(lh) && IS_INF2(rh)) return 1;
	else if(IS_NAN (lh) && IS_NAN (rh)) return 1;

	if(lh.S==rh.S && lh.E==rh.E && (lh.M-rh.M) <= 1)
		return 1;

	return 0;
}

f32_t f32_add(f32_t lh, f32_t rh) {
	f32_t ret;
	__uint32_t M_hidden = 1 << 23;
	__uint32_t mantissa_mask = 0x7FFFFF;
	__uint32_t exponent_mask = 0xFF;

	if(f32_check(lh, '+', rh, &ret)) {
		return ret;
	}

	f32_t max_e, min_e;

	if(lh.E > rh.E || (lh.E == rh.E && lh.M > rh.M)) {
		max_e = lh;
		min_e = rh;
	}
	else {
		max_e = rh;
		min_e = lh;
	}

	__uint32_t delta_e = max_e.E - min_e.E;

	if(max_e.M || max_e.E)
		max_e.M |= M_hidden;
	if(min_e.M || min_e.E)
		min_e.M |= M_hidden;

	// exponent matching
	if(delta_e >= 24) {
		return max_e;
	}

	__int32_t protect_len = 6;

	max_e.M <<= protect_len;
	min_e.M <<= protect_len;
	
	if(delta_e > 0) {
		min_e.M >>= delta_e;
	}

	// calculate mantissa
	ret.M = (max_e.S==min_e.S) ?(max_e.M+min_e.M) :(max_e.M-min_e.M);
	ret.S = max_e.S;
	ret.E = max_e.E;

	// result normalization
	int shift = 0;
	if(ret.M > (M_hidden<<protect_len)) {
		for(shift = 0; (ret.M>>shift) >= (M_hidden << (protect_len+1)); shift++);
	}
	else if(0 == ret.M) {
		ret.E = 0;
		ret.S = 0;
	}
	else if(ret.M < (M_hidden<<protect_len)) {
		for(shift = 0; (ret.M<<shift) < (M_hidden<<protect_len); shift++);
		shift = -shift;
	}

	if(shift) {
		if(shift + protect_len > 0) {
			ret.M = round_to_nearest(ret.M, shift + protect_len);
		}
		else {
			ret.M <<= -(shift + protect_len);
		}

		ret.M &= mantissa_mask;
		ret.E += shift;
	}
	else {
		ret.M = round_to_nearest(ret.M, protect_len);
	}

	// check overflow
	if(ret.E >= exponent_mask) {
		ret.E = exponent_mask;
		ret.M = 0;
	}

	return ret;
}

f32_t f32_mul(f32_t lh, f32_t rh) {
	f32_t ret;
	__uint32_t M_hidden = 1 << 23;
	__uint32_t mantissa_mask = 0x7FFFFF;
	__uint32_t exponent_mask = 0xFF;

	if(f32_check(lh, '*', rh, &ret)) {
		return ret;
	}

	if(0 == lh.E && 0 == lh.M)
		return lh;
	if(0 == rh.E && 0 == rh.M)
		return rh;

	ret.S = 0x1 & (lh.S + rh.S);
	ret.E = lh.E + rh.E - 127;

	__uint64_t M = ((((__uint64_t)lh.M*rh.M) >> 22) + 1) >> 1;
	M += lh.M + rh.M + M_hidden;

	// result normalization
	int shift = 0;
	if(M > M_hidden) {
		for(shift = 0; (M>>shift) >= (M_hidden << 1); shift++);
		ret.M = round_to_nearest(M, shift);
		ret.M &= mantissa_mask;
		ret.E += shift;
	}

	// check overflow
	if(ret.E >= exponent_mask) {
		ret.E = exponent_mask;
		ret.M = 0;
	}

	return ret;
}

void test_f32_add(float f1, float f2) {
	f32_t lh = float_to_f32(f1);
	f32_t rh = float_to_f32(f2);
	float real = f32_to_float(f32_add(lh, rh));
	float expect = f1 + f2;
	if(real != expect) {
		f32_t r32 = float_to_f32(real);
		f32_t e32 = float_to_f32(expect);
		if(!f32_compare(r32, e32)) {
			printf("%f + %f = %f (%f)\n", f1, f2, real, expect);
			printf("[lh] S:%d E:%d M:0x%X\n", lh.S, lh.E, lh.M);
			printf("[rh] S:%d E:%d M:0x%X\n", rh.S, rh.E, rh.M);
			printf("[real] S:%d E:%d M:0x%X\n", r32.S, r32.E, r32.M);
			printf("[expect] S:%d E:%d M:0x%X\n", e32.S, e32.E, e32.M);
			assert(real == expect);
		}
	}
	else {
		//printf("%f + %f = %f (%f)\n", f1, f2, real, expect);
	}
}

void test_f32_mul(float f1, float f2) {
	f32_t lh = float_to_f32(f1);
	f32_t rh = float_to_f32(f2);
	float real = f32_to_float(f32_mul(lh, rh));
	float expect = f1 * f2;
	if(real != expect) {
		f32_t r32 = float_to_f32(real);
		f32_t e32 = float_to_f32(expect);
		if(!f32_compare(r32, e32)) {
			printf("%f x %f = %f (%f)\n", f1, f2, real, expect);
			printf("[lh] S:%d E:%d M:0x%X\n", lh.S, lh.E, lh.M);
			printf("[rh] S:%d E:%d M:0x%X\n", rh.S, rh.E, rh.M);
			printf("[real] S:%d E:%d M:0x%X\n", r32.S, r32.E, r32.M);
			printf("[expect] S:%d E:%d M:0x%X\n", e32.S, e32.E, e32.M);
			assert(real == expect);
		}
	}
	else {
		//printf("%f x %f = %f (%f)\n", f1, f2, real, expect);
	}
}

static float f32_to_bf24(float src) {
	uint32_t value = *(uint32_t*)(&src);

	if(0x7fc00000 == (0x7fc00000 & value)) {
        value = 0x7fc00000;
    }
	else{
		uint32_t lsb = (value >> 8) & 1;
		uint32_t rounding_bias = 0x7f + lsb;
		value += rounding_bias;
		value &= 0xffffff00;
	}
	return *(float*)(&value);
}

void main() {
	//float f1 = 123456;
	//float f2 = 0.123456;

	//f32_t f32 = float_to_f32(f1);

	//printf("%f %f %f\n", f1, f2, f1+f2);
	//printf("S:%d E:%d M:%d\n", f32.S, f32.E, f32.M);
	//printf("%f\n", f32_to_float(float_to_f32(f1)));

	float inf  = f32_to_float((f32_t){0, 0xFF, 0});
	float inf2 = f32_to_float((f32_t){1, 0xFF, 0});
	float nan  = f32_to_float((f32_t){0, 0xFF, 1});
	float nan2 = f32_to_float((f32_t){1, 0xFF, 1});
	float nan3 = f32_to_float((f32_t){0, 0xFF, 0x7FFFFF});
	float nan4 = f32_to_float((f32_t){1, 0xFF, 0x7FFFFF});

	float test_case[] = {
		0, 0.12345, 0.9876, 12345.2356, 234553.5453313, 432665.2323, 1.0, 0.5, 2.0, 1.5, 0.55, 2.5, 1.75,
		-0, -0.12345, -0.9876, -12345.2356, -234553.5453313, -432665.2323, -1.0, -0.5, -2.0, -1.5, -0.55, -2.5, -1.75,
		329221212434345006789012034567800112329.0, -329221212434345006789012034567800112329.0,
		inf, inf2, nan, nan2, nan3, nan4, 1.0, -1.0, 0, 2.0, -2.0, 123, -123,
	};

	/*for(int i = 0; i < sizeof(test_case)/sizeof(float); i++) {
		for(int j = 0; j < sizeof(test_case)/sizeof(float); j++) {
			test_f32_add(test_case[i], test_case[j]);
			test_f32_mul(test_case[i], test_case[j]);
		}
	}*/

	/*test_f32_add(-0.9876, 1.0);
	test_f32_add(1.0, 1.0);
	test_f32_add(0.123450, 0.123450);
	test_f32_add(234553.546875, -432665.218750);

	test_f32_add(1234, -0.123);
	test_f32_add(-1234, 0.123);
	test_f32_add(1234.1223, 1234.5327);
	test_f32_add(0, 0);
	test_f32_add(1234, -1234);
	test_f32_add(223456789012345678, -223456789012345678);*/
	
	float f1,f2,f3;
	
	*(uint32_t*)(&f1) = 0x56b9fe << 8;
	*(uint32_t*)(&f2) = 0xdb8819 << 8;
	
	f32_t lh = float_to_f32(f1);
	f32_t rh = float_to_f32(f2);
	f32_t res = f32_add(lh, rh);
	f3 = f32_to_float(res);
	
	f3 = f32_to_bf24(f3);
	
	printf("%x\n", *(uint32_t*)(&f3));
}
