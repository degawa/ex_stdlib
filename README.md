# Examples of Fortran-stdlib

[fortran-lang](https://fortran-lang.org)が開発している，Fortran用の標準ライブラリ[stdlib](https://github.com/fortran-lang/stdlib)の各モジュールの使用例．

いくつかの使用例は，stdlibの[ドキュメント](https://stdlib.fortran-lang.org/index.html)を参考にしている．

## 要求ソフトウェア
- fpm
- stdlib
- Fortranコンパイラ
    - gfortran
    - Intel oneAPI

## ビルドおよび実行
ビルドおよび実行には[fpm](https://github.com/fortran-lang/fpm)を用いる．

### gfortranによるビルド
```console
fpm build
```
を実行してビルドする．ビルドが成功した後，次のコマンドによってプログラムを実行する．
```console
fpm run
```

### Intel Fortranによるビルド
コンパイラの選択とコンパイラに渡すフラグを設定するオプションが必要である．Linux版のifortでは，`/fpp`ではなく`-fpp`を用いる．

```console
fpm build --compiler ifort --flag "/fpp"
```

```console
fpm run --compiler ifort --flag "/fpp"
```

上記のオプションに加え，`--profile debug`を指定すると，`ex_stdlib_io`の実行の際に警告が表示される．それを避けるには，オプションを`--profile release`に変更する．ただし，その場合はstdlibのビルドに若干の時間を要する．

### 実行時の注意
- `ex_stdlib_error_check()`および`ex_stdlib_error_error_stop()`はプログラムの実行を停止するので，これらの機能の動作を確認する場合を除いて，その呼出しはコメントアウトする．

- Intel Fortranで`--profile debug`を用いる場合，`string_type`の派生型入力は正常に動作しない．

- 各機能の説明および実行結果は，プログラムソース内にコメントとして記述している．

## stdlibの機能
- ascii
    - ASCII文字をパラメータとして定義
    - 文字の検査，文字列の操作の手続を定義
- bitsets
    - 任意桁のビット集合を扱う型と，型に対する手続を定義
- error
    - エラー処理のための手続を定義
- IO
    - ファイル入出力の利便性を改善する手続を定義
- kinds
    - Fortranの型の種別（kind）を表すパラメータを定義
- linalg
    - 線形代数に関係する手続を定義
- logger
    - 実行ログの表示・保存に用いるロガーを派生型として定義
    - 規定のロガーをモジュール変数として定義
- math
    - 多目的用途の数学関数を定義
- optval
    - `optional`引数に標準値を設定する手続を定義
- quadrature 数値積分
    - 数値積分を計算する手続を定義
- random
    - 擬似乱数生成器を定義
- sorting
    - 1次元配列の並べ替えの手続を定義
- specialfunctions
    - 特殊関数を計算する手続を定義
- stats
    - 統計処理に関する手続を定義
- stats_distributions_uniform
    - 一様分布に関する手続を定義
- strings
    - Fortran標準の文字列（`character(len=:)`）操作の手続を定義
- string_type
    - 文字列を扱う派生型を定義
    - 文字列に対する手続を定義
- stringlist_type
    - `string_type`の1次元リストを扱う型を定義
    - `stringlist_type`の要素を参照するための型を定義
- system
    - プログラムの実行を制御する手続を定義

## stdlibの使用例
stdlibの機能を3種類に分類し，モジュールを作成．モジュールサブルーチンを定義し，その中で個別の機能を用いた処理を実装した．

1. system
2. type
3. function

stdlibの分類は，Programming/Algorithms/Mathematicsだが，別観点から分類．

### system
- error
- kinds
- logger
- IO
- system

### type
- ascii
- strings
- string_type
- stringlist_type
- bitsets

### function
- optval
- math
- linalg
- specialfunctions
- quadrature
- random
- stats
- stats_distributions_uniform
- sorting

## サブルーチン一覧
### ex_stdlib_systemモジュール
#### `ex_stdlib_error_check()`
論理値を調べてプログラムを停止する手続`check`の使用例．
- `check`

#### `ex_stdlib_error_error_stop()`
プログラムの実行を停止する手続`error_stop`の使用例．
Fortran 2008の`error stop`の機能不足を補って，Fortran 2018相当の機能を実現する．
- `error_stop`

#### `ex_stdlib_kinds()`
stdlibで定義されている，変数の種別kindの値の使用例．
- `sp`
- `dp`
- `lk`
- `xdp`
- `qp`

#### `ex_stdlib_logger()`
ロガーの使用例．
- `logger_type%configure`
- `logger_type%log_debug`
- `logger_type%log_information`
- `logger_type%log_warning`
- `logger_type%log_error`
- `logger_type%log_io_error`
- `logger_type%log_text_error`
- `logger_type%add_log_file`
- `logger_type%add_log_unit`
- `logger_type%remove_log_unit`

#### `ex_stdlib_io()`
stdlib_ioで定義されている手続`open`, `savetxt`, `loadtxt`の使用例．
- `open`
- `savetxt`
- `loadtxt`

#### `ex_stdlib_system_sleep()`
stdlib_systemで定義されている手続`sleep`の使用例．
- `sleep`

### ex_stdlib_typeモジュール
#### `ex_stdlib_ascii()`
stdlib_asciiで定義されている定数，手続の使用例．
- 定数
    - `TAB`, `NUL`, `LF`, `VT`, `FF`, `CR`, `DEL`
    - `uppercase`
    - `digits`
    - `octal_digits`
    - `fullhex_digits`
    - `hex_digits`
    - `lowerhex_digits`
    - `letters`
- 文字の検査
    - `is_alphanum`
    - `is_alpha`
    - `is_lower`
    - `is_upper`
    - `is_digit`
    - `is_octal_digit`
    - `is_hex_digit`
    - `is_white`
    - `is_blank`
    - `is_control`
    - `is_punctuation`
- 文字列操作
    - `to_lower`
    - `to_upper`
    - `to_title`
    - `to_sentence`
    - `reverse`

#### `subroutine ex_stdlib_strings()`
stdlib_stringsで定義されている，文字列（`character(*)`）操作用の手続の使用例．
- `strip`
- `chomp`
- `starts_with`
- `ends_with`
- `slice`
- `replace_all`
- `padl`
- `padr`
- `count`
- `to_string`

#### ` ex_stdlib_string_type()`
文字列を取り扱う派生型`string_type`の使用例．
- `string_type`
- `=`
- `==`, `/=`, `>=`, `<=`
- `//`
- `len`
- `move`
- `char`
- 派生型IO

#### `ex_stdlib_stringlist_type()`
string_typeのリスト`stringlist_type`の使用例．
- `stringlist_type`
- `stringlist_type%get`
- `stringlist_type%len`
- `stringlist_type%insert_at`
- `stringlist_type%clear`

- `stringlist_index_type`
- `fidx`
- `bidx`

#### `ex_stdlib_bitset()`
ビット集合を表現する派生型`bitset_large`（および`bitset_64`）の使用例．

- `bitset_large`
- `bitset_64`
- `bitset_type%init`
- `bitset_type%bits`
- `bitset_type%bit_count`
- `bitset_type%set`
- `bitset_type%value`
- `bitset_type%test`
- `bitset_type%clear`
- `bitset_type%not`
- `bitset_type%flip`
- `bitset_type%all`
- `bitset_type%from_string`
- `bitset_type%to_string`
- `bitset_type%write_bitset`
- `bitset_type%read_bitset`
- `==`, `>`, `<`
- `and`, `or`, `xor`
- `extract`

### ex_stdlib_functionモジュール
#### ex_stdlib_optval
optional引数がないときに標準値を返す関数optvalの使用例．
- `optval`

#### ex_stdlib_math
stdlib_mathで定義されている手続の使用例．
- `linspace`
- `logspace`
- `arange`
- `clip`
- `gcd`

#### ex_stdlib_linalg
stdlib_linalgで定義されている手続の使用例．
- `diag`
- `eye`
- `trace`
- `outer_product`

#### ex_stdlib_specialfunctions
stdlib_specialfunctionsで定義されている手続の使用例．
- `legendre`

#### ex_stdlib_quadrature
stdlib_quadratureで定義されている手続の使用例．
- `trapz`, `trapz_weights`
- `simps`, `simps_weights`
- `gauss_legendre`
- `gauss_legendre_lobatto`

#### ex_stdlib_random
stdlib_randomで定義されている手続の使用例．
- `random_seed`
- `dist_rand`

#### ex_stdlib_stats
stdlib_statsで定義されている手続の使用例．
- `mean`
- `median`
- `var`
- `cov`
- `corr`
- `moment`

#### ex_stdlib_stats_distribution_uniform
stdlib_stats_distribution_uniformで定義されている手続の使用例．
- `shuffle`
- `rvs_uniform`
- `pdf_uniform`
- `cdf_uniform`

#### ex_stdlib_sorting
stdlib_sortingで定義されている手続の使用例．
- `sort`
- `ord_sort`
- `sort_index`

## サブルーチンの呼出し
モジュール`ex_stdlib_system`, `ex_stdlib_type`, `ex_stdlib_function`をプログラム内でuseし，サブルーチンを呼び出す．

```Fortran
program main
    use :: ex_stdlib_system
    use :: ex_stdlib_type
    use :: ex_stdlib_function
    implicit none

    call ex_stdlib_logger()
    call ex_stdlib_io()
    call ex_stdlib_system_sleep()

    call ex_stdlib_strings()
    call ex_stdlib_string_type()
    call ex_stdlib_stringlist_type()

    call ex_stdlib_optval()
    call ex_stdlib_quadrature()
    call ex_stdlib_sorting()
end program main
```
