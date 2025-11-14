! All rights reserved.
! 
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are
! met:
! 
!  Redistributions of source code must retain the above copyright
! notice, this list of conditions and the following disclaimer.
!   Redistributions in binary form must reproduce the above
! copyright notice, this list of conditions and the following disclaimer
! in the documentation and/or other materials provided with the
! distribution.
!   Neither the name of original developer, nor the names of its
! contributors may be used to endorse or promote products derived from
! this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
! A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
! OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
! SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
! LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
! DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
! THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# define ESCAPE achar(27)
# define ANSI_RED ESCAPE//'[31m'
# define ANSI_GREEN ESCAPE//'[32m'
# define ANSI_NONE ESCAPE//'[0m '
# define ANSI_FAILED ANSI_RED//'[  FAILED  ]'//ANSI_NONE
# define ANSI_PASSED ANSI_GREEN//'[  PASSED  ]'//ANSI_NONE
# define ANSI_OK ANSI_GREEN//'[       OK ]'//ANSI_NONE
# ifdef __GFORTRAN__
#    define MACRO_SAME(A) A
#    define MACRO_CAT2(A,B) MACRO_SAME(A)B
#    define MACRO_CAT3(A,B,C) MACRO_CAT2(A,B)C
#    define MACRO_STRINGIFY(A) "A"
# else
#    define MACRO_CAT2(A,B) A ## B
#    define MACRO_CAT3(A,B,C) A ## B ## C
#    define MACRO_STRINGIFY(A) #A
# endif
# define LOWERCASE(sin, sout) \
do i = 1, len_trim(sin); \
j = iachar(sin(i:i)); \
if (j>= iachar('A') .and. j<=iachar('Z') ) then; \
sout(i:i) = achar(iachar(sin(i:i))+32); \
else; \
sout(i:i) = sin(i:i); \
end if; \
end do;
# define REPORT_FAILURE(expected, actual) \
nfailures = nfailures + 1; \
call cpu_time(toc_assert); \
write(output_unit,'(A, " (", I3, ") : error:")', advance='no') testname, __LINE__; \
call flush(output_unit);write(output_unit,*) ''; \
print*, '   Actual: ', actual; \
print*, ' Expected: ', expected; \
write(*,'(A, A, ". ", A, " line ", I3, " (", E10.3, " ms)")', advance='no') ANSI_FAILED, testcase, testname, __LINE__, toc_assert - tic_assert;\
call flush(output_unit);write(output_unit,*) '';
# define REPORT_SUCCESS \
nsuccess = nsuccess + 1; \
call cpu_time(toc_assert); \
write(output_unit,'(A, A, ". ", A, " line ", I3, " (", E10.3, " ms)")', advance='no') ANSI_OK, testcase, testname, __LINE__, toc_assert - tic_assert;\
call flush(output_unit);write(output_unit,*) '';
# define TESTPROGRAM(name)\
PROGRAMNAME(name) \
USE_ISO_FORTRAN \
IMPLICIT_NONE \
integer :: nfailures_all = 0; \
integer :: nsuccess_all = 0; \
write(output_unit,'("[==========] Running test cases from ", A)', advance='no') __FILE__ ; \
call flush(output_unit);write(output_unit,*) '';
# define PROGRAMNAME(name)\
program name;
# define USE_ISO_FORTRAN \
use, intrinsic :: iso_fortran_env;
# define IMPLICIT_NONE \
implicit none;
# define TEST(case) \
block; \
integer :: nfailures = 0; \
integer :: nsuccess = 0; \
real, allocatable :: tic_case, toc_case; \
character(:), allocatable :: testcase; \
testcase = case; \
write(output_unit,'("[----------] Test ", A)', advance='no') testcase; \
call flush(output_unit);write(output_unit,*) ''; \
block
# define TEST_PRINT(msg) \
write(*,'("[          ] ", A)', advance='no') msg; \
call flush(output_unit);write(output_unit,*) '';
# define START_BLOCK(name) \
block; \
character(*), parameter :: testname = MACRO_STRINGIFY(name); \
real :: tic_assert, toc_assert; \
if(.not. allocated(tic_case)) then ; \
allocate(tic_case, toc_case); \
call cpu_time(tic_case); \
end if; \
call cpu_time(tic_assert) ; \
write(output_unit,'("[ RUN      ] ", A, ". ", A, " at line ", I3)', advance='no') testcase, testname, __LINE__ ; \
call flush(output_unit);write(output_unit,*) '';
# define END_BLOCK(expected, actual, fatal) \
REPORT_SUCCESS \
else; \
REPORT_FAILURE(expected, actual) \
if (fatal) stop 1; \
end if ; \
end block;
# define END_TEST \
call cpu_time(toc_case); \
write(output_unit,'("[----------] Ran ", I3, " tests from ", A)', advance='no') nsuccess + nfailures, testcase; \
call flush(output_unit);write(output_unit,*) ''; \
if(nfailures > 0) then; \
nfailures_all = nfailures_all + nfailures; \
write(output_unit,'(A, I3, " tests from ", A, " (", E10.3, " ms total)")', advance='no') ANSI_FAILED, nfailures, testcase, toc_case - tic_case; \
else; \
nsuccess_all = nsuccess_all + nsuccess; \
write(output_unit,'(A, I3, " tests from ", A, " (", E10.3, " ms total)")', advance='no') ANSI_PASSED, nsuccess, testcase, toc_case - tic_case; \
end if; \
call flush(output_unit);write(output_unit,*) ''; \
end block; \
nsuccess = 0; nfailures = 0; \
end block;
# define END_TESTPROGRAM \
write(output_unit,'("[==========] Ran ", I3, " tests from ", A)', advance='no') nsuccess_all + nfailures_all, __FILE__; \
call flush(output_unit);write(output_unit,*) ''; \
if(nfailures_all > 0) then; \
write(output_unit,'(A, I3, " tests from ", A)', advance='no') ANSI_FAILED, nfailures_all, __FILE__; \
call flush(output_unit);write(output_unit,*) ''; \
stop 1; \
else; \
write(output_unit,'(A, I3, " tests from ", A)', advance='no') ANSI_PASSED, nsuccess_all, __FILE__; \
call flush(output_unit);write(output_unit,*) ''; \
end if; \
end program
/**
  * Macros for testing booleans.
  *
  *    * {ASSERT|EXPECT}_TRUE(condition): Tests that condition .eqv. .true.
  *    * {ASSERT|EXPECT}_FALSE(condition): Tests that condition .eqv. .false.
  *
  */
# define EXPECT_TRUE(condition) \
START_BLOCK(EXPECT_TRUE) \
if(condition .eqv. .true.) then; \
END_BLOCK(.true., condition, .false.)
# define EXPECT_FALSE(condition) \
START_BLOCK(EXPECT_FALSE) \
if(condition .eqv. .false.) then; \
END_BLOCK(.false., condition, .false.)
# define ASSERT_TRUE(condition) \
START_BLOCK(ASSERT_TRUE) \
if(condition .eqv. .true.) then; \
END_BLOCK(.true., condition, .true.)
# define ASSERT_FALSE(condition) \
START_BLOCK(ASSERT_FALSE) \
if(condition .eqv. .false.) then; \
END_BLOCK(.false., condition, .true.)
/**
  * Macros for testing equalities and inequalities.
  *
  *    * {ASSERT|EXPECT}_BEQ(v1, v2): Tests that o1 == o2, in terms of bites
  *    * {ASSERT|EXPECT}_BNEQV(v1, v2): Tests that o1 /= o2, in terms of bites
  *    * {ASSERT|EXPECT}_SAME(v1, v2): Tests that o1 == o2, in terms of reference for derived types
  *    * {ASSERT|EXPECT}_NSAME(v1, v2): Tests that o1 /= o2, in terms of reference for derived types
  *    * {ASSERT|EXPECT}_EQ(v1, v2): Tests that v1 == v2
  *    * {ASSERT|EXPECT}_NE(v1, v2): Tests that v1 != v2
  *    * {ASSERT|EXPECT}_LT(v1, v2): Tests that v1 < v2
  *    * {ASSERT|EXPECT}_LE(v1, v2): Tests that v1 <= v2
  *    * {ASSERT|EXPECT}_GT(v1, v2): Tests that v1 > v2
  *    * {ASSERT|EXPECT}_GE(v1, v2): Tests that v1 >= v2
  *
  * The values must be compatible built-in types,
  * or you will get a compiler error.  By "compatible" we mean that the
  * values can be compared by the respective operator.
  */
# define EXPECT_SAME(val1, val2) \
START_BLOCK(EXPECT_SAME) \
block ; \
logical :: equals_; \
equals_ = same_type_as(val1, val2) .and. loc(val1) == loc(val2) .and. storage_size(val1) == storage_size(val2) ;\
if(equals_) then ; \
END_BLOCK(val2, val1, .false.) \
end block
# define EXPECT_NSAME(val1, val2) \
START_BLOCK(EXPECT_NSAME) \
block ; \
logical :: equals_; \
equals_ = same_type_as(val1, val2) .and. loc(val1) == loc(val2) .and. storage_size(val1) == storage_size(val2) ;\
if(.not. equals_) then; \
END_BLOCK(val2, val1, .false.) \
end block
# define EXPECT_BEQ(val1, val2) \
START_BLOCK(EXPECT_BEQ) \
block ; \
integer(kind=1), allocatable :: mold(:); \
logical :: equals_; \
equals_ = all(transfer(val1, mold) == transfer(val2, mold)); \
if(equals_) then ; \
END_BLOCK(val2, val1, .false.) \
end block
# define EXPECT_BNEQ(val1, val2) \
START_BLOCK(EXPECT_BNEQ) \
block ; \
integer(kind=1), allocatable :: mold(:); \
logical :: equals_; \
equals_ = all(transfer(val1, mold) == transfer(val2, mold)); \
if(.not. equals_) then; \
END_BLOCK(val2, val1, .false.) \
end block
# define EXPECT_EQ(val1, val2) \
START_BLOCK(EXPECT_EQ) \
if(val1 .eq. val2) then; \
END_BLOCK(val2, val1, .false.)
# define EXPECT_NE(val1, val2) \
START_BLOCK(EXPECT_NE) \
if(val1 .ne. val2) then; \
END_BLOCK(val2, val1, .false.)
# define EXPECT_LE(val1, val2) \
START_BLOCK(EXPECT_LE) \
if(val1 .le. val2) then; \
END_BLOCK(val2, val1, .false.)
# define EXPECT_LT(val1, val2) \
START_BLOCK(EXPECT_LT) \
if(val1 .lt. val2) then; \
END_BLOCK(val2, val1, .false.)
# define EXPECT_GE(val1, val2) \
START_BLOCK(EXPECT_GE) \
if(val1 .ge. val2) then; \
END_BLOCK(val2, val1, .false.)
# define EXPECT_GT(val1, val2) \
START_BLOCK(EXPECT_GT) \
if(val1 .gt. val2) then; \
END_BLOCK(val2, val1, .false.)
# define ASSERT_SAME(val1, val2) \
START_BLOCK(ASSERT_SAME) \
block ; \
integer(kind=1), allocatable :: mold(:); \
logical :: equals_; \
equals_ = same_type_as(val1, val2) .and. loc(val1) == loc(val2) .and. storage_size(val1) == storage_size(val2) ;\
if(equals_) equals_ = all(transfer(val1, mold) == transfer(val2, mold)); \
if(equals_) then; \
END_BLOCK(val2, val1, .true.) \
end block
# define ASSERT_NSAME(val1, val2) \
START_BLOCK(ASSERT_NSAME) \
block ; \
integer(kind=1), allocatable :: mold(:); \
logical :: equals_; \
equals_ = same_type_as(val1, val2) .and. loc(val1) == loc(val2) .and. storage_size(val1) == storage_size(val2) ;\
if(equals_) equals_ = all(transfer(val1, mold) == transfer(val2, mold)); \
if(.not. equals_) then; \
END_BLOCK(val2, val1, .true.) \
end block
# define ASSERT_BEQ(val1, val2) \
START_BLOCK(ASSERT_BEQ) \
block ; \
integer(kind=1), allocatable :: mold(:); \
logical :: equals_; \
equals_ = all(transfer(val1, mold) == transfer(val2, mold)); \
if(equals_) then; \
END_BLOCK(val2, val1, .true.) \
end block
# define ASSERT_BNEQ(val1, val2) \
START_BLOCK(ASSERT_BNEQ) \
block ; \
integer(kind=1), allocatable :: mold(:); \
logical :: equals_; \
equals_ = all(transfer(val1, mold) == transfer(val2, mold)); \
if(.not. equals_) then; \
END_BLOCK(val2, val1, .true.) \
end block
# define ASSERT_EQ(val1, val2) \
START_BLOCK(ASSERT_EQ) \
if(val1 .eq. val2) then; \
END_BLOCK(val2, val1, .true.)
# define ASSERT_NE(val1, val2) \
START_BLOCK(ASSERT_NE) \
if(val1 .ne. val2) then; \
END_BLOCK(val2, val1, .true.)
# define ASSERT_LE(val1, val2) \
START_BLOCK(ASSERT_LE) \
if(val1 .le. val2) then; \
END_BLOCK(val2, val1, .true.)
# define ASSERT_LT(val1, val2) \
START_BLOCK(ASSERT_LT) \
if(val1 .lt. val2) then; \
END_BLOCK(val2, val1, .true.)
# define ASSERT_GE(val1, val2) \
START_BLOCK(ASSERT_GE) \
if(val1 .ge. val2) then; \
END_BLOCK(val2, val1, .true.)
# define ASSERT_GT(val1, val2) \
START_BLOCK(ASSERT_GT) \
if(val1 .gt. val2) then; \
END_BLOCK(val2, val1, .true.)
/**
  * String Comparisons.  All tests treat NULL and any non-NULL string
  * as different. Two NULLs are equal.
  *
  *    * {ASSERT|EXPECT}_STREQ(s1, s2):     
  *            Tests that s1 == s2
  *    * {ASSERT|EXPECT}_STRNE(s1, s2):     
  *            Tests that s1 != s2
  *    * {ASSERT|EXPECT}_STRCASEEQ(s1, s2): 
  *            Tests that s1 == s2, ignoring case
  *    * {ASSERT|EXPECT}_STRCASENE(s1, s2): 
  *            Tests that s1 != s2, ignoring case
  */
# define EXPECT_STREQ(s1, s2) \
START_BLOCK(EXPECT_STREQ) \
if(s1 .eq. s2) then; \
END_BLOCK(s2, s1, .false.)
# define EXPECT_STRNE(s1, s2) \
START_BLOCK(EXPECT_STRNE) \
if(s1 .ne. s2) then; \
END_BLOCK(s2, s1, .false.)
# define EXPECT_STRCASEEQ(s1, s2) \
START_BLOCK(EXPECT_STRCASEEQ) \
block; \
character(len_trim(s1)) :: s1l; \
character(len_trim(s2)) :: s2l; \
integer :: i,j; \
LOWERCASE(s1,s1l) \
LOWERCASE(s2,s2l) \
if(s1l .eq. s2l) then; \
END_BLOCK(s2, s1, .false.) \
end block;
# define EXPECT_STRCASENE(s1, s2) \
START_BLOCK(EXPECT_STRCASENE) \
block; \
character(len_trim(s1)) :: s1l; \
character(len_trim(s2)) :: s2l; \
integer :: i,j; \
LOWERCASE(s1,s1l) \
LOWERCASE(s2,s2l) \
if(s1l .ne. s2l) then; \
END_BLOCK(s2, s1, .false.) \
end block;
# define ASSERT_STREQ(s1, s2) \
START_BLOCK(ASSERT_STREQ) \
if(s1 .eq. s2) then; \
END_BLOCK(s2, s1, .true.)
# define ASSERT_STRNE(s1, s2) \
START_BLOCK(ASSERT_STRNE) \
if(s1 .ne. s2) then; \
END_BLOCK(s2, s1, .true.)
# define ASSERT_STRCASEEQ(s1, s2) \
START_BLOCK(ASSERT_STRCASEEQ) \
block; \
character(len_trim(s1)) :: s1l; \
character(len_trim(s2)) :: s2l; \
integer :: i,j; \
LOWERCASE(s1,s1l) \
LOWERCASE(s2,s2l) \
if(s1l .eq. s2l) then; \
END_BLOCK(s2, s1, .true.); \
end block;
# define ASSERT_STRCASENE(s1, s2) \
START_BLOCK(ASSERT_STRCASENE) \
block; \
character(len_trim(s1)) :: s1l; \
character(len_trim(s2)) :: s2l; \
integer :: i,j; \
LOWERCASE(s1,s1l) \
LOWERCASE(s2,s2l) \
if(s1l .ne. s2l) then; \
END_BLOCK(s2, s1, .true.) \
end block;
/**
  * Macros for comparing floating-point numbers.
  *
  *    * {ASSERT|EXPECT}_FLOAT_EQ(val1, val2):
  *         Tests that two float values are almost equal.
  *    * {ASSERT|EXPECT}_DOUBLE_EQ(val1, val2):
  *         Tests that two double values are almost equal.
  *    * {ASSERT|EXPECT}_NEAR(v1, v2, abs_error):
  *         Tests that v1 and v2 are within the given distance to each other.
  */
# define EXPECT_FLOAT_EQ(val1, val2) \
START_BLOCK(EXPECT_FLOAT_EQ) \
if(abs(real(val1, real32) - real(val2, real32)) .le. 2.0*spacing(max(abs(real(val1, real32)),abs(real(val2, real32))))) then; \
END_BLOCK(val2, val1, .false.)
# define EXPECT_FLOAT_NE(val1, val2) \
START_BLOCK(EXPECT_FLOAT_NE) \
if(abs(real(val1, real32) - real(val2, real32)) .gt. 2.0*spacing(max(abs(real(val1, real32)),abs(real(val2, real32))))) then; \
END_BLOCK(val2, val1, .false.)
# define EXPECT_DOUBLE_EQ(val1, val2) \
START_BLOCK(EXPECT_DOUBLE_EQ) \
if(abs(real(val1, real64) - real(val2, real64)) .le. 2.0*spacing(max(abs(real(val1, real64)),abs(real(val2, real64))))) then; \
END_BLOCK(val2, val1, .false.)
# define EXPECT_DOUBLE_NE(val1, val2) \
START_BLOCK(EXPECT_DOUBLE_NE) \
if(abs(real(val1, real64) - real(val2, real64)) .gt. 2.0*spacing(max(abs(real(val1, real64)),abs(real(val2, real64))))) then; \
END_BLOCK(val2, val1, .false.)
# define ASSERT_FLOAT_EQ(val1, val2) \
START_BLOCK(ASSERT_FLOAT_EQ) \
if(abs(real(val1, real32) - real(val2, real32)) .le. 2.0*spacing(max(abs(real(val1, real32)),abs(real(val2, real32))))) then; \
END_BLOCK(val2, val1, .true.)
# define ASSERT_FLOAT_NE(val1, val2) \
START_BLOCK(ASSERT_FLOAT_NE) \
if(abs(real(val1, real32) - real(val2, real32)) .gt. 2.0*spacing(max(abs(real(val1, real32)),abs(real(val2, real32))))) then; \
END_BLOCK(val2, val1, .true.)
# define ASSERT_DOUBLE_EQ(val1, val2) \
START_BLOCK(ASSERT_DOUBLE_EQ) \
if(abs(real(val1, real64) - real(val2, real64)) .le. 2.0*spacing(max(abs(real(val1, real64)),abs(real(val2, real64))))) then; \
END_BLOCK(val2, val1, .true.)
# define ASSERT_DOUBLE_NE(val1, val2) \
START_BLOCK(ASSERT_DOUBLE_NE) \
if(abs(real(val1, real64) - real(val2, real64)) .gt. 2.0*spacing(max(abs(real(val1, real64)),abs(real(val2, real64))))) then; \
END_BLOCK(val2, val1, .true.)
# define EXPECT_NEAR(val1, val2, abs_error) \
START_BLOCK(EXPECT_NEAR) \
if(abs(val1 - val2) < abs_error) then; \
END_BLOCK(val2, val1, .false.)
# define ASSERT_NEAR(val1, val2, abs_error) \
START_BLOCK(ASSERT_NEAR) \
if(abs(val1 - val2) < abs_error) then; \
END_BLOCK(val2, val1, .true.)