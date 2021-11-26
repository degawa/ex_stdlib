module ex_stdlib_function
    use, intrinsic :: iso_fortran_env
    use :: stdlib_optval
    use :: stdlib_math
    use :: stdlib_linalg
    use :: stdlib_specialfunctions
    use :: stdlib_quadrature
    use :: stdlib_random
    use :: stdlib_stats
    use :: stdlib_stats_distribution_uniform
    use :: stdlib_sorting
    implicit none
    private
    public :: ex_stdlib_optval
    public :: ex_stdlib_math
    public :: ex_stdlib_linalg
    public :: ex_stdlib_specialfunctions
    public :: ex_stdlib_quadrature
    public :: ex_stdlib_random
    public :: ex_stdlib_stats
    public :: ex_stdlib_stats_distribution_uniform
    public :: ex_stdlib_sorting

contains
    !| optional引数がないときに標準値を返す関数optvalの使用例．
    subroutine ex_stdlib_optval()
        implicit none

        ! 1 0
        print *, optval(1, default=0), optval(default=0)

        ! 1.00000000 0.00000000
        print *, optval(1., default=0.), optval(default=0.)

        ! 1.0000000000000000 0.0000000000000000
        print *, optval(1d0, default=0d0), optval(default=0d0)

        ! T F
        print *, optval(.true., default=.false.), optval(default=.false.)

        ! (1.00000000,2.00000000) (0.00000000,0.00000000)
        print *, optval((1., 2.), default=(0., 0.)), optval(default=(0., 0.))

        block
            real(real64) :: x = 64.

            ! 8.0000000000000000 3.9999999999999996
            print *, root(x), root(x, 3)
        end block
    contains
        !| xのn乗根を計算する．
        real(real64) function root(x, n)
            implicit none
            real(real64), intent(in) :: x
                !! 被開平数
            integer(int32), intent(in), optional :: n
                !! 指数

            root = x**(1d0/optval(n, 2)) ! nが無いときは2が使われる
        end function root
    end subroutine ex_stdlib_optval

    !| stdlib_mathで定義されている手続の使用例．
    subroutine ex_stdlib_math()
        implicit none

        block
            ! [start, end]を等間隔に分割した数を配列で返す．
            ! 戻り値の型は，[start,end]の型と同じ．

            ! 0.00000000      0.200000003      0.400000006      0.600000024      0.800000012       1.00000000
            print *, linspace(start=0., end=1., n=6)

            ! 0.0000000000000000 0.20000000000000001 0.40000000000000002 0.60000000000000009 0.80000000000000004 1.0000000000000000
            print *, linspace(0d0, 1d0, 6)

            ! 整数型で範囲を指定すると，戻り値は倍精度実数
            ! 0.0000000000000000 0.20000000000000001 0.40000000000000002 0.60000000000000009 0.80000000000000004 1.0000000000000000
            print *, linspace(0, 1, 6)
        end block

        block
            complex(real64) :: start, end
            complex(real64), allocatable :: z(:)
            start = cmplx(1d0, 0d0)
            end = cmplx(0d0, 1d0)

            ! (1.0000000000000000,0.0000000000000000) (0.80000000000000004,0.20000000000000001) (0.59999999999999998,0.40000000000000002)
            ! (0.39999999999999991,0.60000000000000009) (0.19999999999999996,0.80000000000000004) (0.0000000000000000,1.0000000000000000)
            z = linspace(start, end, 6)
            print *, z
        end block

        block

            ! [base**start, base**end]の範囲を対数で分割する数を配列で返す．
            ! 戻り値の型は，基本的に[start,end]あるいはbaseの型と同じ．

            ! baseが倍精度実数なので倍精度実数で返す．
            ! 1.0000000000000000E-003   1.0000000000000000E-002  0.10000000000000001        1.0000000000000000        10.000000000000000
            print *, logspace(-3, 1, n=5, base=10d0)

            ! start, endが単精度実数なので単精度実数で返す．
            ! 1.00000005E-03   9.99999978E-03  0.100000001       1.00000000       10.0000000
            print *, logspace(-3., 1., n=5, base=10)

            ! start, end, baseが全て整数なら，整数で値を返す．
            !  0           0           0           1          10
            print *, logspace(start=-3, end=1, n=5, base=10)
        end block

        block
            ! 指定された区間を等間隔に分割した値をもつ配列で返す．

            ! 引数を整数で指定する．
            ! 引数が一つの場合は終端として扱い，始点を1，間隔を1として扱う．
            ! 1           2           3
            print *, arange(3) ! = arange(1,3,1)

            ! 終端が負の場合，始点は1，間隔を-1として扱う．
            ! 1           0          -1
            print *, arange(-1) ! = arange(1,-1,-1)

            ! 始点と終点を指定．間隔を1として扱う．
            ! 0           1           2
            print *, arange(0, 2) ! = arange(0, 2, 1)

            ! 始点，終点，間隔を指定．
            ! 0           2
            print *, arange(0, 2, 2)

            ! 引数を実数で指定する．
            ! 引数が一つの場合は終端として扱い，始点を1，間隔を1として扱う．
            ! 1.00000000       2.00000000       3.00000000
            print *, arange(3.0) ! = arange(1.0,3.0,1.0)

            ! 終端が負の場合，始点は1，間隔を-1として扱う．
            ! 1.00000000       0.00000000      -1.00000000
            print *, arange(-1.0) ! = arange(1.0,-1.0,-1.0)

            ! 始点と終点を指定．間隔を1として扱う．
            ! 0.00000000       1.00000000       2.00000000
            print *, arange(0.0, 2.0) ! = arange(0.0, 2.0, 1.0)

            ! 始点，終点，間隔を指定．
            ! 0.00000000      0.200000003
            print *, arange(0.0, 0.2, 0.2)
        end block

        block
            ! 指定された数値の範囲[xmin, xmax]から外れた入力値xに対して，最も近い範囲内の値を返す．

            ! [-5, 5]の範囲から外れた値10に対して5を返す．
            ! 5
            print *, clip(10, -5, 5)

            ! [0.0, 0.1]の範囲から外れた値-1.0に対して0.0を返す．
            ! 0.00000000
            print *, clip(-1.0, 0.0, 0.1)
        end block

        block
            ! 二つの数の最大公約数を計算する．
            ! 6
            print *, gcd(48, 18)
        end block
    end subroutine ex_stdlib_math

    !| stdlib_linalgで定義されている手続の使用例．
    subroutine ex_stdlib_linalg()
        implicit none

        block
            real(real32), allocatable :: v(:)
            real(real32), allocatable :: A(:, :)

            ! ベクトル（1次元配列）を入力として，その値を対角成分にもつ行列を返す．
            ! [1, 1]を対角成分として行列を作る．
            !  i\j 1 2
            !    +-----
            !   1| 1 0
            !   2| 0 1
            v = [1, 1]
            A = diag(v)
            print *, A

            ! [1,2,3,4]を対角成分にもつ行列を返す．
            !  i\j 1  2  3  4
            !    +-----------
            !   1| 1  0  0  0
            !   2| 0  2  0  0
            !   3| 0  0  3  0
            !   4| 0  0  0  4
            A = diag(arange(1, 4))
            print *, A

            block
                real(real32), allocatable :: u(:)
                real(real32), allocatable :: l(:)
                real(real32), allocatable :: d(:)

                allocate (l(3), source=-1.)
                allocate (u(3), source=1.)
                allocate (d(4), source=-2.)

                ! 対角線を1下にずらし，[-1, -1, -1]を成分にもつ行列を返す．
                !  i\j 1  2  3  4
                !    +-----------
                !   1| 0  0  0  0
                !   2|-1  0  0  0
                !   3| 0 -1  0  0
                !   4| 0  0 -1  0
                A = diag(l, -1)
                print *, A

                ! 対角線を1上にずらし，[1, 1, 1]を成分にもつ行列を返す．
                !  i\j 1  2  3  4
                !    +-----------
                !   1| 0  1  0  0
                !   2| 0  0  1  0
                !   3| 0  0  0  1
                !   4| 0  0  0  0
                A = diag(u, 1)
                print *, A

                !  i\j 1  2  3  4
                !    +-----------
                !   1| 2  1  0  0
                !   2|-1  2  1  0
                !   3| 0 -1  2  1
                !   4| 0  0 -1  2
                A = diag(l, -1) + diag(d) + diag(u, 1)
                print *, A
            end block
        end block

        block
            ! 単位行列を返す．

            ! 2行2列の単位行列を返す．
            !  i\j 1 2
            !    +-----
            !   1| 1 0
            !   2| 0 1
            print *, eye(2)

            ! 3行3列の単位行列を返す．
            !  i\j 1  2  3
            !    +--------
            !   1| 1  0  0
            !   2| 0  1  0
            !   3| 0  0  1
            print *, eye(3)

            ! 2行1列の単位行列を返す
            !  i\j 1
            !    +--
            !   1| 1
            !   2| 0
            print *, eye(2, 1)

            ! 3行3列の単位行列を返す．
            !  i\j 1  2  3
            !    +--------
            !   1| 1  0  0
            !   2| 0  1  0
            !   3| 0  0  1
            print *, eye(3, 3)

            ! 3行2列の単位行列を返す．
            !  i\j 1  2
            !    +-----
            !   1| 1  0
            !   2| 0  1
            !   3| 0  0
            print *, eye(3, 2)
        end block

        block
            real(real32) :: A(3, 3)

            ! 行列のtraceを計算する．
            !  i\j 1  2  3
            !    +--------
            !   1| 1  0  0
            !   2| 0  1  0
            !   3| 0  0  1
            ! 3
            A = eye(3, 3)
            print *, trace(A)

            !  i\j 1  2  3
            !    +--------
            !   1| 1  4  7
            !   2| 2  5  8
            !   3| 3  6  9
            ! 15
            A = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
            print *, trace(A)
        end block

        block
            real(real32), allocatable :: A(:, :), u(:), v(:)

            ! 直積u×v^Tを計算する．
            !  i\j  1  2  3
            !    +---------
            !   1|  4  5  6
            !   2|  8 10 12
            !   3| 12 15 18
            u = [real :: 1, 2, 3]
            v = [real :: 4, 5, 6]
            A = outer_product(u, v)
            print *, A
        end block
    end subroutine ex_stdlib_linalg

    !| stdlib_specialfunctionsで定義されている手続の使用例．
    subroutine ex_stdlib_specialfunctions()
        implicit none

        block
            real(real64) :: x
            x = 1d-1

            ! ルジャンドル多項式
            ! P_0(x) = 1
            ! 1.0000000000000000        1.0000000000000000
            print *, legendre(0, x), 1d0

            ! P_1(x) = x
            ! 0.10000000000000001       0.10000000000000001
            print *, legendre(1, x), x ! x

            ! P_2(x) = (3d0*x**2 - 1d0)/2d0
            ! -0.48499999999999999      -0.48499999999999999
            print *, legendre(2, x), (3d0*x**2 - 1d0)/2d0

            ! P_3(x) = (5d0*x**3 - 3d0*x)/2d0
            ! -0.14749999999999999      -0.14750000000000002
            print *, legendre(3, x), (5d0*x**3 - 3d0*x)/2d0

            ! P_10(x) = (46189d0*x**10 - 109395d0*x**8 + 90090d0*x**6 - 30030d0*x**4 + 3465d0*x**2 - 63d0)/256d0
            ! -0.12212499738710939      -0.12212499738710936
            print *, legendre(10, x), (46189d0*x**10 - 109395d0*x**8 + 90090d0*x**6 - 30030d0*x**4 + 3465d0*x**2 - 63d0)/256d0
        end block
    end subroutine ex_stdlib_specialfunctions

    !| stdlib_quadratureで定義されている手続の使用例．
    subroutine ex_stdlib_quadrature()
        implicit none

        real(real32) :: x(6), f(6)
        real(real32) :: w(6)

        x = linspace(-1., 1., 6)
        f = 5.*x**4
        ! x = [-1.0 -0.6 -0.2 0.2 0.6 1.0 ]
        ! f = [5.0 0.648 8E-03 8E-03  0.648 5.0 ]

        block
            ! 台形則で関数値fを積分する．
            ! 関数値と座標値を指定する．
            ! 2.52479982
            print *, trapz(f, x)

            ! 関数値とそのサンプリング点の間隔を指定する．
            ! 2.52480006
            print *, trapz(f, dx=0.4)

            ! サンプリング点の間隔の指定誤り．
            ! 3.15600014
            print *, trapz(f, dx=0.5)
        end block

        block
            ! 与えられたx座標値に対して，重み係数を返す．
            ! 0.199999988      0.400000006      0.400000036      0.400000006      0.399999976      0.199999988
            w = trapz_weights(x)
            print *, w

            ! 重み係数wと関数値fを用いて積分する．
            ! 0.999999940      0.259200007       3.19999922E-03   3.20000271E-03  0.259199977      0.999999940
            ! 2.52480006
            print *, f*w
            print *, sum(f*w)
        end block

        block
            ! シンプソン則で関数値fを積分する．
            ! 関数値と座標値を指定する．
            ! 2.05973339
            print *, simps(f, x)

            ! 関数値とそのサンプリング点の間隔を指定する．
            ! 2.05973339
            print *, simps(f, dx=0.4)

            ! サンプリング点の間隔の指定誤り．
            ! 2.57466650
            print *, simps(f, dx=0.5)
        end block

        block
            ! 与えられたx座標値に対して，重み係数を返す．
            ! 0.141666651      0.491666645      0.366666734      0.366666734      0.491666645      0.141666681
            w = simps_weights(x)
            print *, w

            ! 重み係数wと関数値fを用いて積分する．
            ! 0.708333254      0.318599999       2.93333293E-03   2.93333642E-03  0.318599999      0.708333373
            ! 2.05973339
            print *, f*w
            print *, sum(f*w)
        end block

        block
            real(real64) :: x(6), w(6)

            ! Gauss-Legendre求積法のノードと重みを計算する．
            ! -0.93246951420315205      -0.66120938646626448      -0.23861918608319693       0.23861918608319693       0.66120938646626448       0.93246951420315205
            ! 0.17132449237917019       0.36076157304813866       0.46791393457269126       0.46791393457269126       0.36076157304813866       0.17132449237917019
            call gauss_legendre(x, w)
            print *, x
            print *, w

            ! ノードと重みを使って，xが[-1, 1]の範囲で5x^4を積分する．
            ! 1.9999999999999987
            print *, sum(5.*x**4*w)
        end block

        block
            real(real64) :: x(6), w(6)

            ! Gauss-Legendre-Lobatto求積法のノードと重みを計算する．
            ! -1.0000000000000000      -0.76505532392946474      -0.28523151648064510       0.28523151648064510       0.76505532392946474        1.0000000000000000
            ! 6.6666666666666666E-002  0.37847495629784678       0.55485837703548646       0.55485837703548646       0.37847495629784678        6.6666666666666666E-002
            call gauss_legendre_lobatto(x, w)
            print *, x
            print *, w

            ! ノードと重みを使って，xが[-1, 1]の範囲で5x^4を積分する．
            ! 1.9999999999999993
            print *, sum(5.*x**4*w)
        end block
    end subroutine ex_stdlib_quadrature

    !| stdlib_randomで定義されている手続の使用例．
    subroutine ex_stdlib_random()
        implicit none

        integer(int32) :: seed_put, seed_get

        ! 乱数の種を設定・取得する．
        seed_put = 135792468
        call random_seed(seed_put, seed_get)

        ! 入力された整数のkindに基づいて，その整数の範囲で擬似乱数を生成する．
        ! 1バイト整数の範囲 [-2^7, 2^7 -1]内で乱数を生成する．
        ! -90
        print *, dist_rand(0_int8)

        ! 2バイト整数の範囲 [-2^15, 2^15 -1]内で乱数を生成する．
        ! -32725
        print *, dist_rand(0_int16)

        ! 4バイト整数の範囲 [-2^31, 2^31 -1]内で乱数を生成する．
        ! -1601563881
        print *, dist_rand(0_int32)

        ! 8バイト整数の範囲 [-2^63, 2^63 -1]内で乱数を生成する．
        ! 180977695517992208
        print *, dist_rand(0_int64)
    end subroutine ex_stdlib_random

    !| stdlib_statsで定義されている手続の使用例．
    subroutine ex_stdlib_stats()
        implicit none
        real(real32) :: x(1:6) = [1., 2., 3., 4., 5., 6.]
        real(real32) :: y(1:2, 1:3) = reshape([1., 2., 3., 4., 5., 6.], [2, 3])

        ! x = [1,2,3,4,5,6]^T
        ! y =
        !   i\j 1  2  3
        !     +--------
        !    1| 1  3  5
        !    2| 2  4  6

        block
            ! 平均値を計算する．
            ! 配列の全要素の平均を計算する．=(1+2+3+4+5+6)/6
            ! 3.50000000
            print *, mean(x)

            ! 配列の全要素のうち，3より大きい値の平均を計算する．=(4_5+6)/3
            ! 5.00000000
            print *, mean(x, mask=(x > 3.))

            ! 配列の全要素の平均を計算する．=(1+2+3+4+5+6)/6
            ! 3.50000000
            print *, mean(y)

            ! 配列の第1次元方向成分の平均を計算する．=[(1+2)/2, (3+4)/2, (5+6)/2]
            ! 1.50000000       3.50000000       5.50000000
            print *, mean(y, dim=1)

            ! 配列の第1次元方向成分のうち，3より大きい値の平均を計算する．=[存在しない, 4/1, (5+6)/2]
            ! 成分が0個の場合，0/0のためNaNになる．
            ! NaN   4.00000000       5.50000000
            print *, mean(y, dim=1, mask=(y > 3.))
        end block

        block
            real(real32) :: x(1:6) = [1., 1., 6., 7., 8., 9.]
            real(real32) :: y(1:3, 1:2) = reshape([0., 1., 6., 7., 8., 9.], [3, 2])
            ! x = [1,1,6,7,8,9]^T
            ! y =
            !   i\j 1  2
            !     +-----
            !    1| 0  7
            !    2| 1  8
            !    3| 6  9

            ! 中央値を計算する．
            ! 配列の全要素の成分から中央値を計算する．=(6+7)/2
            ! 6.50000000
            print *, median(x)

            ! 配列の全要素の成分のうち6より大きい値[7,8,9]の中央値を計算する．
            ! 8.00000000
            print *, median(x, mask=(x > 6.))

            ! 配列の全要素の成分から中央値を計算する．=(6+7)/2
            ! 6.50000000
            print *, median(y)

            ! 配列の第1次元方向成分の中央値を計算する．= [median([0,1,6]), median(7,8,9)]
            ! 1.00000000       8.00000000
            print *, median(y, dim=1)

            ! 配列の第1次元方向成分のうち，3より大きい値の中央値を計算する．=[median([6]), median(7,8,9)]
            ! 6.00000000       8.00000000
            print *, median(y, dim=1, mask=(y > 3.))
        end block

        block
            ! 配列の分散varianceを計算する．
            ! 計算式はvar(array) = 1/(n-1) sum_i (array(i) - mean(array))^2, nは要素数．

            ! x = [1,2,3,4,5,6]^T
            ! y =
            !   i\j 1  2  3
            !     +--------
            !    1| 1  3  5
            !    2| 2  4  6

            ! 全要素の分散を計算する．
            ! 3.50000000
            print *, var(x)

            ! corrected=.false.の場合(n-1)ではなくnでスケーリングする．
            ! 2.91666675
            print *, var(x, corrected=.false.)

            ! 全要素の分散を計算する．
            ! 3.50000000
            print *, var(y)

            ! 配列の第1次元方向成分の分散を計算する．=[var([1,2]), var([3,4]), var([5,6])]
            ! 0.500000000      0.500000000      0.500000000
            print *, var(y, dim=1)

            ! 配列の第1次元方向成分のうち，3より大きい値の分散を計算する．=[var([]), var([4]), var([5,6])]
            ! NaN              NaN  0.500000000
            print *, var(y, dim=1, mask=y > 3.)

            ! 配列の第1次元方向成分のうち，3より大きい値の分散を計算する．=[var([]), var([4]), var([5,6])]
            ! n-1ではなくnでスケーリングする．
            ! NaN   0.00000000      0.250000000
            print *, var(y, dim=1, mask=y > 3., corrected=.false.)
        end block

        block
            ! 指定された配列の次元における共分散covarianceを計算する．
            ! 計算式はcov(array) = 1/(n-1) sum_i (array(i) - mean(array) * (array(i) - mean(array))), nは要素数．

            ! x = [1,2,3,4,5,6]^T
            ! y =
            !   i\j 1  2  3
            !     +--------
            !    1| 1  3  5
            !    2| 2  4  6

            ! 全要素の共分散を計算する．
            ! 3.50000000
            print *, cov(x, dim=1)

            ! n-1ではなくnでスケーリングする．
            ! 2.91666675
            print *, cov(x, dim=1, corrected=.false.)

            ! 配列の第1次元方向成分の分散を計算する．
            ! yの場合，計算中に用いているmeanが3成分の配列を返すので，各次元で3個の共分散が得られ，合計で3x3=9個の値が返される．
            ! 0.500000000      0.500000000      0.500000000      0.500000000      0.500000000      0.500000000      0.500000000      0.500000000      0.500000000
            print *, cov(y, dim=1)
        end block

        block
            real(real32) :: y(1:2, 1:3) = reshape([-1., 40., -3., 4., 10., 6.], [2, 3])

            ! x = [1,2,3,4,5,6]^T
            ! y =
            !   i\j  1  2   3
            !     +----------
            !    1| -1 -3  10
            !    2| 40  4   6

            ! Pearsonの相関係数を計算する．
            ! 計算式はcorr(x, y) = cov(x, y) / sqrt( var(x) * var(y))

            ! 全要素を用いて相関係数を計算する．
            ! 1.00000000
            print *, corr(x, dim=1)

            ! 配列の第1次元方向成分の分散を計算する．
            ! yの場合，計算中に用いているcovが9成分の配列を返すので，corrの戻り値も9個になる．
            ! 1.00000000       1.00000000     -0.999999940       1.00000000       1.00000000     -0.999999940      -1.00000000      -1.00000000      0.999999940
            print *, corr(y, dim=1)

            ! 配列の第2次元方向成分の分散を計算する．
            ! 計算中に用いているcovが4成分の配列を返すので，corrの戻り値も4個になる．
            ! 1.00000000     -0.324803889     -0.324803889      0.999999940
            print *, corr(y, dim=2)
        end block

        block

            ! x = [1,2,3,4,5,6]^T

            ! k次のモーメントを計算する．
            ! 計算式は moment(array) = 1/n sum_i (array(i) - mean(array))^k
            ! 中心が与えられている時の計算式はmoment(array) = 1/n sum_i (array(i) - center)^k

            ! 配列の全要素を用いて2次のモーメントを計算する．
            ! 2.91666675
            print *, moment(x, order=2)

            ! 配列の全要素を用いて2次のモーメントを計算する．
            ! 2.91666675
            print *, moment(y, order=2)

            ! 配列の第1次元方向成分を用いて2次のモーメントを計算する．=[moment([1,2], 2), moment([3,4], 2), moment([5,6], 2)]
            ! 0.250000000      0.250000000      0.250000000
            print *, moment(y, order=2, dim=1)

            ! 配列の第1次元方向成分のうち，3より大きい値を用いて2次のモーメントを計算する．=[moment([], 2), moment([4], 2), moment([5,6], 2)]
            ! NaN   0.00000000      0.250000000
            print *, moment(y, 2, 1, mask=(y > 3.))

            ! 中心を与えて2次のモーメントを計算する．
            ! 15.1666670
            print *, moment(x, 2, center=0.)

            ! 中心を与えて，配列の第1次元方向成分を用いて2次のモーメントを計算する．=[moment([1,2], 2, 0), moment([3,4], 2, 0), moment([5,6], 2, 0)]
            ! 1.50000000       3.50000000       5.50000000
            print *, moment(y, 1, 1, center=0.)
        end block
    end subroutine ex_stdlib_stats

    !| stdlib_stats_distribution_uniformで定義されている手続の使用例．
    subroutine ex_stdlib_stats_distribution_uniform()
        use :: stdlib_math
        implicit none
        real(real32) :: x(10)
        integer(int32) :: n(10)
        complex(real32) :: z(10)
        integer :: seed_put, seed_get

        n = arange(10)
        x = arange(10.)
        z = linspace(cmplx(1., 1.), cmplx(10., 10.), 10)
        ! n = [1 2 3 4 5 6 7 8 9 10]
        ! x = [1. 2. 3. 4. 5. 6. 7. 8. 9. 10.]
        ! z = [(1.,1.) (2.,2.) (3.,3.) (4.,4.) (5.,5.) (6.,6.) (7.,7.) (8.,8.) (9..9.) (10.,10.)]

        block
            seed_put = 32165498
            call random_seed(seed_put, seed_get)

            ! Fisher-Yatesアルゴリズムに基づいてランダムに並び替える．

            ! 整数を並び替える．
            ! 10 6 9 2 8 1 3 5 7 4
            print *, shuffle(n)

            ! 実数を並び替える．
            ! 5. 10. 9. 4. 3. 8. 2. 1. 7. 6.
            print *, shuffle(x)

            ! 複素数を並び替える．
            ! (8.,8.) (7.,7.) (4.,4.) (1.,1.) (5.,5.) (9.,9.) (6.,6.) (3.,3.) (2.,2.) (10.,10.)
            print *, shuffle(z)
        end block

        block

            seed_put = 1234567
            call random_seed(seed_put, seed_get)

            ! [0,1]の範囲で一様分布を返す．

            ! 引数を何も指定しなければ，[0,1]の範囲で，単精度実数として値を返す．
            ! 0.161520019
            print *, rvs_uniform()

            ! scaleが指定されると，[0,scale]の範囲で一様分布を返す．
            ! scaleの型によって戻り値の型が決定される．
            ! [0,3.]の範囲で単精度実数値を返す．
            ! 1.65974522
            print *, rvs_uniform(scale=3.0)

            ! [0,20]の範囲で整数値を返す．
            ! 14
            print *, rvs_uniform(scale=20)

            ! locとscaleが指定されると，[loc,loc+scale]の範囲で一様分布を返す．
            ! locおよびscaleの型によって戻り値の型が決定される．
            ! [-0.5,0.5]の範囲で値を返す．
            ! 0.486900032
            print *, rvs_uniform(loc=-0.5, scale=1.0)

            ! arrayを指定すると，array_sizeの数だけ値を返す．
            ! -0.885760069     -0.219811499     -0.145343125     -0.535613418     -0.715533257     -0.251231968     -0.698165953     -0.662991643     -0.431156635     -0.403834939
            print *, rvs_uniform(loc=-1.0, scale=1.0, array_size=10) ![-1.,1.]

            block
                real :: a(3, 4, 5), b(3, 4, 5)

                ! [-0.5, 0.5]の範囲で，60個の値を返す．
                a = -0.5
                b = 1.0
                print *, rvs_uniform(loc=a, scale=b)
            end block
        end block

        block

            seed_put = 1234567
            call random_seed(seed_put, seed_get)

            ! 一様分布の確率密度関数を計算する．

            ! [2, 12]の範囲で，x=3における確率密度関数を計算する．
            ! 9.09090936E-02
            print *, pdf_uniform(x=3, loc=2, scale=10)

            ! [0, 1]の範囲で，x=0.5における確率密度関数を計算する．
            ! 1.00000000
            print *, pdf_uniform(0.5, 0.0, 1.0)

            ! [-1, 1]の範囲で，x=0.7における確率密度関数を計算する．
            ! 0.500000000
            print *, pdf_uniform(0.7, -1.0, 2.0)

            block
                real(real32) :: a(3, 4, 5), b(3, 4, 5), x(3, 4, 5)

                a(:, :, :) = 0.
                b(:, :, :) = 2.
                ! [0,2]の範囲で連続分布の値を60個取得する．
                x = reshape(rvs_uniform(0., 2., size(a)), shape(a))
                ! [0,2]の範囲で，xにおける確率密度関数を60個計算する．
                print *, pdf_uniform(x, a, b)
            end block
        end block

        block

            seed_put = 1234567
            call random_seed(seed_put, seed_get)

            ! 累積分布関数を計算する．
            ! [0, 1]の範囲で，x=0.5における累積分布関数を計算する．
            ! 0.500000000
            print *, cdf_uniform(x=0.5, loc=0., scale=1.)

            ! [-1.0, 1.0]の範囲で，x=0.7における累積分布関数を計算する
            ! 0.850000024
            print *, cdf_uniform(0.7, -1.0, 2.0)

            ! [2, 12]の範囲で，x=6における累積分布関数を計算する
            ! 0.454545468
            print *, cdf_uniform(x=6, loc=2, scale=10) ! a cumulative at 6 in [2, 10]

            block
                real(real32) :: a(3, 4, 5), b(3, 4, 5), x(3, 4, 5)

                a(:, :, :) = -1.
                b(:, :, :) = 2.
                ! [0,2]の範囲で連続分布の値を60個取得する．
                x = reshape(rvs_uniform(-1.0, 2.0, size(a)), shape(a))
                ! [0,2]の範囲で，xにおける累積分布関数を60個計算する．
                print *, cdf_uniform(x, a, b)
            end block
        end block
    end subroutine ex_stdlib_stats_distribution_uniform

    !| stdlib_sortingで定義されている手続の使用例．
    subroutine ex_stdlib_sorting()
        implicit none

        integer(int32), allocatable :: array(:)
        array = [5, 4, 3, 1, 10, 4, 9]

        block
            ! イントロソートを利用して並び替える．
            ! 1 3 4 4 5 9 10
            call sort(array)
            print *, array

            ! イントロソートを利用して逆順に並び替える．
            ! 10 9 5 4 4 3 1
            call sort(array, reverse=.true.)
            print *, array
        end block

        block
            integer(int32), allocatable :: work(:)

            array = [5, 4, 3, 1, 10, 4, 9]
            allocate (work, mold=array)

            ! Rustのソートアルゴリズムを利用して並び替える．
            ! 1 3 4 4 5 9 10
            call ord_sort(array, work)
            print *, array

            ! Rustのソートアルゴリズムを利用して逆順に並び替える．
            ! 10 9 5 4 4 3 1
            call ord_sort(array, work, reverse=.true.)
            print *, array
        end block

        block
            integer(int32), allocatable :: work(:)
            integer(int_size), allocatable :: index(:), iwork(:)

            array = [5, 4, 3, 1, 10, 4, 9]
            allocate (index(1:size(array)))
            allocate (work(1:size(array)/2))
            allocate (iwork(1:size(array)/2))

            ! ord_sortを利用して並び替えのインデックスを返す．
            call sort_index(array, index, work, iwork)
            ! 1 3 4 4 5 9 10
            ! 4 3 2 6 1 7 5
            print *, array
            print *, index

            ! indexを用いて並び替える．
            array = [5, 4, 3, 1, 10, 4, 9]
            array = array(index)
            ! array(1) = array(index(1)) = array(4) = 1
            ! array(2) = array(index(2)) = array(3) = 3
            ! array(3) = array(index(3)) = array(2) = 4
            ! array(4) = array(index(4)) = array(6) = 4
            ! array(5) = array(index(5)) = array(1) = 5
            ! array(6) = array(index(6)) = array(7) = 9
            ! array(7) = array(index(7)) = array(5) = 10
            ! 1 3 4 4 5 9 10
            print *, array
        end block
    end subroutine ex_stdlib_sorting
end module ex_stdlib_function
