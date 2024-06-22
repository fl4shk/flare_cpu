using uint8_t = unsigned char;
using uint16_t = unsigned short int;
using uint32_t = unsigned int;
using uint64_t = unsigned long long int;

using int8_t = signed char;
using int16_t = short int;
using int32_t = int;
using int64_t = long long int;

extern "C" {
//--------
static constexpr uint32_t TEST_ARR_SIZE = 0x100u;
extern uint32_t test_arr[TEST_ARR_SIZE];

uint32_t sum_u32(
	uint32_t* /*__restrict*/ arr, uint32_t size
) {
	uint32_t ret = 0u;
	for (uint32_t i=0; i<size; ++i) {
		ret += arr[i];
	}
	return ret;
}
//void infin(void) {
//	for (;;) {
//	}
//}
//--------
} // extern "C"
