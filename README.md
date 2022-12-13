# yaft 
[![CI tests](https://github.com/shamazmazum/yaft/actions/workflows/test.yml/badge.svg)](https://github.com/shamazmazum/yaft/actions/workflows/test.yml)

## Yet another common lisp Fourier transform library

Yaft is another FFT library written purely in Common Lisp. It specializes on
one-dimensional simple arrays of `(complex double-float)` (FFT) or
`double-float` (RFFT) numbers and has some features absent in another FFT
implementations. Here is a comparison with some other popular libraries:

|  Feature                   |  Yaft         | fftpack5  | napa-fft3    |
|----------------------------|---------------|-----------|--------------|
| Complex FFT                |    ✓          |     ✗     |     ✓        |
| Real FFT                   |    ✓¹         |     ✓     |     ✓        |
| Not power-of-2 FFT         |    ✓          |     ✓     |     ✗        |
| No side-effects²           |    ✓          |     ✗     |     ✗        |
| Fast                       |  Not really³  |     ✓     |     ✓        |
| Prime number of elements   |    ✓          |     ✗     |     ✗        |
| Multidimensional arrays    |    ✗          |     ✗     |     ✗        |

Some additional explanation:

1. yaft only supports RFFT with even number of elements.
2. napa-fft3 and fftpack5 make cache of twiddle factors in some internal
   buffers. This can make these libraries unsuitable for multithreaded
   environment. This library works without any side-effects with exception of
   consing and signalling conditions.
3. Let's compare execution times of yaft and fftpack5:


``` lisp
CL-USER> (time
 (loop with array = (make-array 40000
                                :element-type 'double-float
                                :initial-contents
                                (loop repeat 40000 collect (random 1d0)))
       repeat 100 do (yaft:rfft array)))
Evaluation took:
  1.757 seconds of real time
  1.756551 seconds of total run time (1.756551 user, 0.000000 system)
  [ Run times consist of 0.009 seconds GC time, and 1.748 seconds non-GC time. ]
  100.00% CPU
  6,676,822,452 processor cycles
  3,657,752,016 bytes consed
  
NIL
CL-USER> (time
 (loop with array = (make-array 40000
                                :element-type 'single-float
                                :initial-contents
                                (loop repeat 40000 collect (random 1f0)))
       repeat 100 do (fftpack5:rfft array)))
Evaluation took:
  0.617 seconds of real time
  0.617910 seconds of total run time (0.617910 user, 0.000000 system)
  100.16% CPU
  2,347,576,464 processor cycles
  32,946,352 bytes consed

NIL
```

As you can see can see, fftpack5 is about 3 times faster than yaft. But if a
number of elements in the array has large prime factors, yaft is much faster:

``` lisp
CL-USER> (time
 (loop with array = (make-array 19997
                                :element-type '(complex double-float)
                                :initial-contents (loop repeat 19997 collect
                                                        (complex (random 1d0)
                                                                 (random 1d0))))
       repeat 20 do (yaft:fft array yaft:+forward+)))
Evaluation took:
  3.443 seconds of real time
  3.442248 seconds of total run time (3.434396 user, 0.007852 system)
  [ Run times consist of 0.017 seconds GC time, and 3.426 seconds non-GC time. ]
  99.97% CPU
  13,086,727,204 processor cycles
  7,183,863,024 bytes consed
  
NIL
CL-USER> (time
 (loop with array = (make-array 19997
                                :element-type 'single-float
                                :initial-contents
                                (loop repeat 19997 collect (random 1f0)))
       repeat 20 do (fftpack5:rfft array)))
Evaluation took:
  20.123 seconds of real time
  20.122875 seconds of total run time (20.122875 user, 0.000000 system)
  100.00% CPU
  76,466,978,086 processor cycles
  3,548,224 bytes consed
  
NIL
```

## What does yaft return?

Yaft returns unnormalized FFT transform and its inverse is the original array
multiplied by a number of elements:

``` lisp
CL-USER> (yaft:irfft
 (let ((array (make-array 10
                          :element-type 'double-float
                          :initial-contents (loop for x below 10 collect
                                                  (float x 0d0)))))
   (yaft:rfft array))
 10)
#(2.0872192862952943d-14 10.000000000000007d0 20.00000000000001d0 30.0d0
  40.00000000000001d0 49.999999999999986d0 60.0d0 69.99999999999999d0 80.0d0
  90.0d0)
```

## Interface

``` lisp
yaft:fft
yaft:+forward+
yaft:+inverse+

yaft:rfft
yaft:irfft
```
