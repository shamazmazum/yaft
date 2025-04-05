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
| Fast                       |    ✓          |     ✗     |     ✓        |
| Prime number of elements   |    ✓          |     ✗     |     ✗        |
| Multidimensional arrays    |    ✗          |     ✗     |     ✗        |

Some additional explanation:

1. yaft only supports RFFT with even number of elements.
2. napa-fft3 and fftpack5 make cache of twiddle factors in some internal
   buffers. This can make these libraries unsuitable for multithreaded
   environment. This library works without any side-effects with exception of
   consing and signalling conditions.

Benchmarks can be run by loading `yaft/benchmark` system and running
`(yaft/benchmark:benchmark)`. A small excerpt:

~~~~
Complex FFT
Sequence length = 90100
-                SAMPLES  TOTAL       MINIMUM   MAXIMUM   MEDIAN    AVERAGE     DEVIATION
REAL-TIME        100      3.552284    0.033916  0.040123  0.035425  0.035523    0.00111
RUN-TIME         100      3.552644    0.033916  0.040124  0.035424  0.035526    0.001115

Real FFT
Sequence length = 95100
-                SAMPLES  TOTAL      MINIMUM   MAXIMUM   MEDIAN    AVERAGE    DEVIATION
REAL-TIME        100      1.662827   0.016386  0.018972  0.016525  0.016628   0.000388
RUN-TIME         100      1.663096   0.016385  0.018973  0.016525  0.016631   0.000389
~~~~

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
