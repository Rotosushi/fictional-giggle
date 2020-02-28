//#pragma once
//
//typedef unsigned int _flags;
//
//// helper function for flag extraction
//bool is_nth_bit_set(unsigned int value, short nth) {
//	if (nth >= 32) return false;
//	/* 
//	sanity check:
//		because a short can represent values
//		so much greater than the number of bits
//		that represent value, it is statistically
//		more likely for a wrong value to be
//		passed into nth, than a right one.
//	*/
//	return ((value >> nth) & 1) == 1;
//}
//// helper function for flag setting
//void set_nth_bit_to(unsigned int& value, short nth, bool direction) {
//	value = (value & ~(1 << nth)) | (direction << nth);
//}
//
//void set_nth_bit(unsigned int& value, short nth) {
//	value |= (1 << nth);
//}
//
//void clear_nth_bit(unsigned int& value, short nth) {
//	value &= ~(1 << nth);
//}
