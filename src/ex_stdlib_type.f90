module ex_stdlib_type
    use, intrinsic :: iso_fortran_env
    use :: stdlib_ascii
    use :: stdlib_strings
    use :: stdlib_bitsets
    use :: stdlib_string_type
    use :: stdlib_stringlist_type
    use :: stdlib_io
    implicit none
    private
    public :: ex_stdlib_ascii
    public :: ex_stdlib_strings
    public :: ex_stdlib_string_type
    public :: ex_stdlib_stringlist_type
    public :: ex_stdlib_bitset

contains
    !| stdlib_asciiで定義されている定数，手続の使用例．
    subroutine ex_stdlib_ascii()
        implicit none

        block
            ! a .. z
            print *, "lowercase: "//TAB//TAB, lowercase

            ! A .. Z
            print *, "uppercase: "//TAB//TAB, uppercase

            ! 0 .. 9
            print *, "ditigs: "//TAB//TAB, digits

            ! 0 .. 7
            print *, "octal digits: "//TAB//TAB, octal_digits

            ! 0 .. 9A .. Fa .. f
            print *, "fullhex digits:"//TAB, fullhex_digits

            ! 0 .. 9a .. f
            print *, "hex digits: "//TAB//TAB, hex_digits

            ! 0 .. 9a .. f
            print *, "lowerhex digits:"//TAB, lowerhex_digits

            ! A .. Za .. z
            print *, "letters:"//TAB//TAB, letters
        end block

        block
            ! 文字がアルファベットもしくは数字かを判別する．
            print *, "A is alphabet or number :", is_alphanum("A") ! T
            print *, "a is alphabet or number :", is_alphanum("a") ! T
            print *, "1 is alphabet or number :", is_alphanum("1") ! T
            print *, "# is alphabet or number :", is_alphanum("#") ! F
            print *, "á is alphabet or number :", is_alphanum("á") ! F
        end block

        block
            ! 文字がアルファベットかを判別する．
            print *, "A is alphabet :", is_alpha("A") ! T
            print *, "A is alphabet :", is_alpha("a") ! T
            print *, "1 is alphabet :", is_alpha("1") ! F
            print *, "# is alphabet :", is_alpha("#") ! F
            print *, "á is alphabet :", is_alpha("á") ! F
        end block

        block
            ! 文字が小文字アルファベットかを判別する．
            print *, "A is lowercase alphabet :", is_lower("A") ! F
            print *, "a is lowercase alphabet :", is_lower("a") ! T
            print *, "1 is lowercase alphabet :", is_lower("1") ! F
            print *, "# is lowercase alphabet :", is_lower("#") ! F
            print *, "á is lowercase alphabet :", is_lower("á") ! F
        end block

        block
            ! 文字が大文字アルファベットかを判別する．
            print *, "A is uppercase alphabet :", is_upper("A") ! T
            print *, "a is uppercase alphabet :", is_upper("a") ! F
            print *, "1 is uppercase alphabet :", is_upper("1") ! F
            print *, "# is uppercase alphabet :", is_upper("#") ! F
            print *, "á is uppercase alphabet :", is_upper("á") ! F
        end block

        block
            ! 文字が数字かを判別する．
            print *, "A is digit :", is_digit("A") ! F
            print *, "a is digit :", is_digit("a") ! F
            print *, "1 is digit :", is_digit("1") ! T
        end block

        block
            ! 文字が8進数で使われる数字かを判別する．
            print *, "A is octal digit :", is_octal_digit("A") ! F
            print *, "a is octal digit :", is_octal_digit("a") ! F
            print *, "7 is octal digit :", is_octal_digit("7") ! T
            print *, "8 is octal digit :", is_octal_digit("8") ! F
        end block

        block
            ! 文字が16進数で使われる数字かを判別する．
            print *, "A is hex digit :", is_hex_digit("A") ! T
            print *, "a is hex digit :", is_hex_digit("a") ! T
            print *, "F is hex digit :", is_hex_digit("F") ! T
            print *, "G is hex digit :", is_hex_digit("G") ! F
            print *, "8 is hex digit :", is_hex_digit("8") ! T
        end block

        block
            ! 文字がホワイトスペースかを判別する．
            ! space, tab, vertical tab, form feed, carriage return, linefeedのとき真
            print *, "' ' is white space :", is_white(" ") ! T
            print *, "NUL is white space :", is_white(NUL) ! F
            print *, "TAB is white space :", is_white(TAB) ! T
            print *, "LF is white space :", is_white(LF) ! T
            print *, "VT is white space :", is_white(VT) ! T
            print *, "FF is white space :", is_white(FF) ! T
            print *, "CR is white space :", is_white(CR) ! T
            print *, "A is white space :", is_white("A") ! F
        end block

        block
            ! 文字がブランク文字かを判別する．
            ! space, tabのとき真．
            print *, "' ' is blank :", is_blank(" ") ! T
            print *, "NUL is blank :", is_blank(NUL) ! F
            print *, "TAB is blank :", is_blank(TAB) ! T
            print *, "LF is blank :", is_blank(LF) ! F
            print *, "VT is blank :", is_blank(VT) ! F
            print *, "FF is blank :", is_blank(FF) ! F
            print *, "CR is blank :", is_blank(CR) ! F
            print *, "A is blank :", is_blank("A") ! F
        end block

        block
            ! 文字が制御文字かを判別する．
            print *, "' ' is control character :", is_control(" ") ! F
            print *, "NUL is control character :", is_control(NUL) ! T
            print *, "TAB is control character :", is_control(TAB) ! T
            print *, "LF is control character :", is_control(LF) ! T
            print *, "VT is control character :", is_control(VT) ! T
            print *, "FF is control character :", is_control(FF) ! T
            print *, "CR is control character :", is_control(CR) ! T
            print *, "DEL is control character :", is_control(DEL) ! T
            print *, "A is control character :", is_control("A") ! F
        end block

        block
            ! 文字が句読文字かを判別する．
            print *, ", is punctuation character :", is_punctuation(",") ! T
            print *, ". is punctuation character :", is_punctuation(".") ! T
            print *, ": is punctuation character :", is_punctuation(":") ! T
            print *, "; is punctuation character :", is_punctuation(";") ! T
            print *, "! is punctuation character :", is_punctuation("!") ! T
            print *, "# is punctuation character :", is_punctuation("#") ! T
            print *, "~ is punctuation character :", is_punctuation("~") ! T
            print *, "+ is punctuation character :", is_punctuation("+") ! T
            print *, "_ is punctuation character :", is_punctuation("_") ! T
            print *, "' ' is punctuation character :", is_punctuation(" ") ! F
            print *, "A is punctuation character :", is_punctuation("A") ! F
            print *, "1 is punctuation character :", is_punctuation("1") ! F
            print *, "TAB is punctuation character :", is_punctuation(TAB) ! F
        end block

        block
            ! 大文字アルファベットを小文字に変換する．
            ! ABCDEFGHIJKLMNOPQRSTUVWXYZ
            ! abcdefghijklmnopqrstuvwxyz
            print *, uppercase
            print *, to_lower(uppercase)

            ! 小文字アルファベットを大文字に変換する．
            ! abcdefghijklmnopqrstuvwxyz
            ! ABCDEFGHIJKLMNOPQRSTUVWXYZ
            print *, lowercase
            print *, to_upper(lowercase)
        end block

        block
            character(*), parameter :: str = "hello there. this is 'enquated string'. 19th century writer"

            ! 各英単語の最初の一文字を大文字にする．
            ! 単語が引用符などで囲まれている場合でも，最初の単語を大文字にする．
            ! 最初が数字の場合は何もしない．
            ! Hello There. This Is 'Enquated String'. 19th Century Writer
            print *, to_title(str)

            ! 文字列の最初の一文字を大文字にする．
            ! Hello there. this is 'enquated string'. 19th century writer
            print *, to_sentence(str)

            ! 文字列を反転する．
            ! retirw yrutnec ht91 .'gnirts detauqne' si siht .ereht olleh
            print *, reverse(str)
        end block
    end subroutine ex_stdlib_ascii

    !| stdlib_stringsで定義されている，文字列（`character(*)`）操作用の手続の使用例．
    subroutine ex_stdlib_strings()
        implicit none

        block
            ! 文字列前後の空白を削除する．
            ! |hello|
            print '(A)', "|"//strip("   hello   ")//"|"

            ! whitespaceは全て削除する．
            ! ||
            print '(A)', "|"//strip(" "//TAB//LF//VT//FF//CR)//"|"

            ! 文字列の先頭が空白でない場合は，whitespaceを削除しない．
            !|'
            !'|
            print '(A)', "|"//strip("'"//TAB//LF//VT//FF//CR//"'")//"|"
        end block

        block
            ! 文字列の末尾から指定した文字を削除する．文字を指定しない場合は空白が削除される．
            ! |   hello|
            print '(A)', "|"//chomp("   hello   ")//"|"

            ! 文字列からlとoを削除する．
            ! |he|
            print '(A)', "|"//chomp("hello", set=["l", "o"])//"|"

            ! lとoが末尾にないので変化しない．
            ! |   hello   |
            print '(A)', "|"//chomp("   hello   ", set=["l", "o"])//"|"

            ! l, o, " "が末尾にあるので削除される．
            ! |   he|
            print '(A)', "|"//chomp("   hello   ", set=["l", "o", " "])//"|"

            ! loが末尾にあるので削除される．
            ! |hel|
            print '(A)', "|"//chomp("hello", substring="lo")//"|"
        end block

        block
            ! 文字列がsubstringから始まっているかを判別．

            ! hello starts with hel: T
            print *, "hello starts with hel: ", starts_with("hello", "hel")

            ! hello starts with lo: F
            print *, "hello starts with lo: ", starts_with("hello", "lo")

            ! 文字列がsubstringで終わっているかを判別．

            ! hello ends with hel: F
            print *, "hello ends with hel: ", ends_with("hello", "hel")

            ! hello ends with lo: T
            print *, "hello ends with lo: ", ends_with("hello", "lo")
        end block

        block
            ! 文字列を取り出す．
            ! 先頭から末尾まで間隔2で文字列を取り出す．
            ! 13579
            print '(A)', slice("1234567890", stride=2)

            ! 2文字目から末尾まで間隔2で文字列を取り出す．
            ! 24680
            print '(A)', slice("1234567890", first=2, stride=2)

            ! 先頭から4文字目まで間隔2で文字列を取り出す．
            ! 13
            print '(A)', slice("1234567890", last=4, stride=2)

            ! 3文字目から9文字目まで間隔4で文字列を取り出す．
            ! 37
            print '(A)', slice("1234567890", first=3, last=9, stride=4)
        end block

        block
            ! 文字列から部分文字列を別の文字列に置き換える．
            ! ingをedに置き換える．
            ! functions for stred handled
            print '(A)', replace_all("functions for string handling", pattern="ing", replacement="ed")

            ! wwを12に置き換える．
            ! 121212
            print '(A)', replace_all("wwwwww", "ww", "12")

            ! "  "を" "に置き換える．オーバーラップはしない．
            ! |   |
            print '(A)', "|"//replace_all("      ", "  ", " ")//"|"
        end block

        block
            ! 指定の文字数になるように左側に文字を詰める．
            ! 10文字になるように左側に空白を詰める．
            ! |    string|
            print '(A)', "|"//padl("string", 10)//"|"

            ! 10文字になるように左側に指定の文字を詰める．
            ! |----string|
            print '(A)', "|"//padl("string", 10, "-")//"|"

            ! 指定の文字数が元の文字数より小さい場合は何もしない．
            ! |string|
            print '(A)', "|"//padl("string", 4, "-")//"|"

            ! 指定の文字数になるように右側に文字を詰める．
            ! 10文字になるように右側に空白を詰める．
            ! |string    |
            print '(A)', "|"//padr("string", 10)//"|"

            ! 10文字になるように右側に指定の文字を詰める．
            ! |string----|
            print '(A)', "|"//padr("string", 10, "-")//"|"

            ! 指定の文字数が元の文字数より小さい場合は何もしない．
            ! |string|
            print '(A)', "|"//padr("string", 4, "-")//"|"
        end block

        block
            ! 文字列に含まれる部分文字列の数を数える．
            ! iの数を数える．
            ! 3
            print *, count("functions for string handling", "i")

            ! ingの数を数える．
            ! 2
            print *, count("functions for string handling", "ing")

            ! i, n, oの数を数える．
            ! [3, 5, 2]
            print *, count("functions for string handling", ["i", "n", "o"])

            ! "  "の数を数える．オーバーラップを許容し，1,2文字目，2,3文字目･･･の数を返す．
            ! 5
            print *, count("      ", "  ")

            ! "  "の数を数える．引数でオーバーラップを許容することを明示する．
            ! 5
            print *, count("      ", "  ", consider_overlapping=.true.)

            ! "  "の数を数える．オーバーラップを許容しない．
            ! 1,2文字目がカウントされたら，次は2,3文字目ではなく3,4文字目をカウントする．
            ! 3
            print *, count("      ", "  ", consider_overlapping=.false.)
        end block

        block
            ! 整数を文字に変換する．
            ! 2147483647
            print *, to_string(huge(0))

            ! -2147483647
            print *, to_string(-huge(0))

            ! 書式付きで整数を文字に変換する

            ! 4桁の数として文字列に変換する．
            ! |   1
            print *, "|"//to_string(1, '(I4)')

            ! 4桁でゼロ埋めして文字列に変換する．
            ! |0001
            print *, "|"//to_string(1, '(I4.4)')

            ! 2桁の数として文字に変換する．桁数が足りない場合は*に置き換えられる．
            ! **
            print *, to_string(100, '(I2)')

            block
                integer(int32) :: i
                i = 16
                ! 2桁の10進数として文字列に変換．
                ! 16
                print *, to_string(i, '(I2)')

                ! 8桁の2進数として文字列に変換．
                ! 00010000
                print *, to_string(i, '(B8.8)')

                ! 2桁の16進数として文字列に変換．
                ! 10
                print *, to_string(i, '(Z2)')
            end block
        end block

        block
            ! 実数を文字列に変換する．
            ! 0.340282347E+39
            print *, to_string(huge(0.))

            ! -0.340282347E+39
            print *, to_string(-huge(0.))

            ! 1.00000000
            print *, to_string(1.)

            ! 書式付きで実数を文字列に変換する．
            ! 全体で8文字，浮動小数点以下を3桁として文字列に変換する．
            ! 100.000 100.000
            print '(A,F8.3)', to_string(100., '(F8.3)'), 100.

            ! 指数表記で文字列に変換する．
            ! 0.10E+04  0.10E+04
            print '(A,E10.2e2)', to_string(1000., '(E10.2e2)'), 1000.

            ! 科学表記で文字列に変換する．
            ! 1.00E+03  1.00E+03
            print '(A,ES10.2)', to_string(1000., '(ES10.2)'), 1000.
        end block

        block
            ! 論理型を文字列に変換する．

            ! 偽を文字列に変換する．
            ! F
            print *, to_string(.false.)

            ! 真を文字列に変換する．
            ! T
            print *, to_string(.true.)

            ! 書式付きで文字列に変換する．
            !   T
            print *, to_string(.true., '(L3)')

            ! 誤った書式で文字列に変換すると，[*]に置き換えられる．
            ! [*]
            print *, to_string(.true., '(I3)')
        end block

        block
            complex(real64) :: c
            c = cmplx(0.75, 0.25)

            ! 複素数を文字列に変換する．
            ! 書式付きで複素数を文字列に変換する．
            !  (  0.75,  0.25)
            print *, to_string(c, '(F6.2)')

            ! 複素数リテラルを文字列に変換する．
            ! (0.500000000,0.500000000)
            print *, to_string((0.5, 0.5))

            ! 書式付きで複素数リテラルを文字列に変換する．
            ! (   0.500,   0.500)
            print *, to_string((0.5, 0.5), '(F8.3)')
        end block
    end subroutine ex_stdlib_strings

    !| 文字列を取り扱う派生型`string_type`の使用例．
    subroutine ex_stdlib_string_type()
        implicit none

        block
            type(string_type) :: str

            ! コンストラクタを利用した初期化．
            str = string_type("string")

            ! 派生型IOおよびlen関数が利用可能．
            ! string           6
            print *, str, len(str)

            ! 文字列リテラルに基づいて初期化．
            ! string_type          11
            str = "string_type"
            print *, str, len(str)

            ! 整数リテラルに基づいて初期化．
            ! -2147483647
            str = string_type(-huge(0))
            print *, str

            ! 論理値リテラルに基づいて初期化．
            ! T
            str = string_type(.true.)
            print *, str
        end block

        block
            type(string_type) :: str1, str2

            str1 = "string"

            ! string_type間の代入
            ! string
            str2 = str1
            print *, str2

            ! 二つのstring_typeの比較．
            ! T F T T
            print *, str1 == str2, str1 /= str2, str1 >= str2, str1 <= str2

            ! 二つのstring_typeの連結．
            ! stringstring
            print *, str1//str2

            str2 = ""

            ! string_type間でデータを移動．
            !  string           6           0
            call move(from=str1, to=str2)
            print *, str2, len(str2), len(str1)
        end block

        block
            type(string_type) :: str
            character(:), allocatable :: chr
            character, allocatable :: chr_array(:)
            str = "string"

            ! 文字列に変換
            ! string
            chr = char(str)
            print *, chr

            ! 範囲を指定して文字列に変換
            ! trin
            chr = char(str, start=2, last=5)
            print *, chr

            ! string_type内の指定した位置の文字を文字の配列に変換．
            ! ["s", "t", "n", "g"]
            chr_array = char(str, [1, 2, 5, 6])
            print *, chr_array

            ! stdlib_asciiで定義されている関数の一部が利用可能．
            !  STRING string gnirts
            print *, to_upper(str), to_lower(to_upper(str)), reverse(str)
        end block

        block
            use :: stdlib_io
            integer(int32) :: unit
            type(string_type) :: strw, strr
            strw = "string"

            ! 書式付き派生型IOの利用．
            ! string
            print '(DT"string_type")', strw

            unit = open ("string_type.txt", "wt")
            ! 書式付き派生型IOでのファイル出力
            write (unit, '(DT"string_type")') strw ! string_typeの内容（"string"）を出力して改行．
            write (unit, *) "" ! 現状では，もう1行改行がないと，string_typeとして読み込めない．
            close (unit)
            ! string_type.txtの内容
            ! ---
            ! string
            ! ↵
            ! ↵
            ! ---

            unit = open ("string_type.txt", "rt")
            ! 派生型IOでのファイル入力．書式付き派生型入力は利用できない．
            read (unit, *) strr
            close (unit)

            ! stringstring
            print '(2DT"string_type")', strw, strr
        end block
    end subroutine ex_stdlib_string_type

    !| string_typeのリスト`stringlist_type`の使用例．
    subroutine ex_stdlib_stringlist_type()
        implicit none

        block
            type(stringlist_type) :: strlist
            type(string_type) :: str

            ! コンストラクタを利用したstringlistの初期化．
            ! 全ての要素（string_type）が同じ長さ．character(2) :: char(3)で再現可能．
            ! 要素の指定には，fidx(先頭からの要素番号)もしくはbidx(末尾からの要素番号)の戻り値を利用する．
            ! #1 #2 #3
            strlist = stringlist_type(["#1", "#2", "#3"])
            print *, strlist%get(fidx(1)), strlist%get(fidx(2)), strlist%get(fidx(3))

            ! コンストラクタを利用したstringlistの初期化．
            ! 要素（string_type）の長さが異なる．
            ! one two three
            strlist = stringlist_type([string_type("one"), string_type("two"), string_type("three")])
            print *, strlist%get(fidx(1)), strlist%get(fidx(2)), strlist%get(fidx(3))

            ! stringlist_typeの型束縛手続きlenは，リストの長さ（要素数）を返す．
            ! 3
            print *, strlist%len()

            ! stringlist_typeの要素を取り出してstring_typeに代入．
            ! three
            str = strlist%get(fidx(3))
            print *, str
        end block

        block
            type(stringlist_type) :: strlist
            type(stringlist_index_type) :: idx

            ! one two three
            strlist = stringlist_type([string_type("one"), string_type("two"), string_type("three")])
            print *, strlist%get(fidx(1)), strlist%get(fidx(2)), strlist%get(fidx(3))

            ! stringlist_typeの先頭からの要素を取得するためのインデックス．
            ! 先頭要素はfidx(1)に代わってlist_headを利用可能．
            idx = fidx(1)

            ! stringlist_typeの先頭に要素を追加．
            ! four one two three
            ! 4
            call strlist%insert_at(idx, string_type("four"))
            print *, strlist%get(fidx(1)), strlist%get(fidx(2)), strlist%get(fidx(3)), strlist%get(fidx(4))
            print *, strlist%len()

            ! stringlist_typeの末尾からの要素を取得するためのインデックス．
            ! 末端要素はbidx(1)に代わってlist_tailを利用可能．
            idx = bidx(1)

            ! stringlist_typeの末尾に要素を追加．
            ! four one two three five
            ! 5
            call strlist%insert_at(idx, string_type("five"))
            print *, strlist%get(fidx(1)), strlist%get(fidx(2)), strlist%get(fidx(3)), strlist%get(fidx(4)), strlist%get(fidx(5))
            print *, strlist%len()

            ! stringlist_typeを初期化
            ! 0
            call strlist%clear()
            print *, strlist%len()
        end block

        block
            type(stringlist_type) :: strlist1, strlist2, strlist
            strlist1 = stringlist_type([string_type("one"), string_type("two"), string_type("three")])
            strlist2 = stringlist_type([string_type("four"), string_type("five"), string_type("six")])

            ! stringlist_type同士の比較．
            ! F T
            print *, strlist1 == strlist2, strlist1 /= strlist2

            ! stringlist_typeの連結．
            ! 6
            ! three four
            strlist = strlist1//strlist2
            print *, strlist%len()
            print *, strlist%get(fidx(3)), strlist%get(fidx(4))
        end block
    end subroutine ex_stdlib_stringlist_type

    !| ビット集合を表現する派生型`bitset_large`（および`bitset_64`）の使用例．
    subroutine ex_stdlib_bitset()
        implicit none
        type(bitset_64) :: bitset
        type(bitset_64) :: bitset2

        ! ビット集合を64ビット（64桁）で初期化．
        call bitset%init(64)

        ! ビット集合のビット数，全てが0かの判別，全てが1かの判別．
        ! 64 T F
        print *, bitset%bits(), bitset%none(), bitset%all()

        ! 範囲を指定してビット集合のビットを1に設定．
        ! ビットの桁番号は0始まり．
        call bitset%set(0, 63)

        ! 全てが1かの判別，31ビット目が1かの判別，31ビット目の値の取得
        ! T T 1
        print *, bitset%all(), bitset%test(31), bitset%value(31)

        ! 1のビットの数をカウント
        ! 64
        print *, bitset%bit_count()

        block
            character(:), allocatable :: str

            ! ビット集合の値を文字列に変換．
            ! 1111111111111111111111111111111111111111111111111111111111111111
            call bitset%to_string(str)
            print *, str
        end block

        block
            integer(int32) :: unit
            unit = open ("bitset64.txt", "wt")

            ! ビット集合の値を指定の装置に書き出し．
            call bitset%write_bitset(unit)
            close (unit)

            ! bitset64.txtの内容
            ! ---
            ! S64B1111111111111111111111111111111111111111111111111111111111111111
            ! ↵
            ! ---

            ! ビット集合のリテラルは二つの要素から構成されている．
            ! ビット数リテラルはSから始まり，10進数の数字（0~9）で構成される桁数が続く．
            ! ビット値リテラルはBから始まり，2進数の数字（0,1）で構成されるビット列が，ビット数分続く．

            unit = open ("bitset64.txt", "rt")

            ! ビット集合の値を指定の装置から読み込み．
            call bitset2%read_bitset(unit)
            close (unit)

            ! 全ビット数が1かを判定．
            ! T
            print *, bitset2%all()

            ! 出力したビット集合と読み込んだビット集合を比較
            ! T
            print *, bitset == bitset2
        end block

        block
            character(:), allocatable :: str1, str2

            ! 範囲を指定してビット集合のビットを初期化する．
            call bitset%clear(0, 63)
            call bitset2%clear(0, 63)

            call bitset%set(0, 47)
            call bitset2%set(32, 63)
            call bitset%to_string(str1)
            call bitset2%to_string(str2)

            ! 0000000000000000111111111111111111111111111111111111111111111111
            ! 1111111111111111111111111111111100000000000000000000000000000000
            print *, str1
            print *, str2

            ! ビット集合を比較
            ! F T
            print *, bitset > bitset2, bitset < bitset2

            ! ビット集合同士のandを計算し，第1引数に上書き．
            ! 0000000000000000111111111111111100000000000000000000000000000000
            call and(bitset, bitset2)
            call bitset%to_string(str1)
            print *, str1

            ! bitsetの値を設定し直し．
            ! 0000000000000000111111111111111111111111111111111111111111111111
            call bitset%clear(0, 63)
            call bitset%set(0, 47)

            ! ビット集合同士のorを計算し，第1引数に上書き．
            ! 1111111111111111111111111111111111111111111111111111111111111111
            call or(bitset, bitset2)
            call bitset%to_string(str1)
            print *, str1

            ! bitsetの値を設定し直し．
            ! 0000000000000000111111111111111111111111111111111111111111111111
            call bitset%clear(0, 63)
            call bitset%set(0, 47)

            ! ビット集合同士のxorを計算し，第1引数に上書き．
            ! 1111111111111111000000000000000011111111111111111111111111111111
            call xor(bitset, bitset2)
            call bitset%to_string(str1)
            print *, str1

            ! bitsetの値を設定し直し．
            ! 0000000000000000111111111111111111111111111111111111111111111111
            call bitset%init(64)
            call bitset%set(0, 47)

            ! 否定を計算して上書き．
            ! 1111111111111111000000000000000000000000000000000000000000000000
            call bitset%not()
            call bitset%to_string(str1)
            print *, str1

            ! 範囲を指定してbitset2のビット（1111111111111111111111111111111100000000000000000000000000000000）を反転．
            ! 0000000000000000000000000000000011111111111111111111111111111111
            call bitset2%flip(0, 63)
            call bitset2%to_string(str2)
            print *, str2
        end block

        block
            type(bitset_large) :: bitset_l1, bitset_l2
            character(:), allocatable :: str1, str2

            ! bitset_largeを128ビットで初期化
            call bitset_l1%init(128)
            call bitset_l2%init(128)

            ! ビット集合のビット数，全てが0かの判別，全てが1かの判別．
            ! 128 128
            ! F F
            ! T T
            print *, bitset_l1%bits(), bitset_l2%bits()
            print *, bitset_l1%all(), bitset_l2%all()
            print *, bitset_l1%none(), bitset_l2%none()

            ! 範囲を指定してビットを反転
            ! 00000000000000000000000000000000000000000000000000000000000000001111111111111111111111111111111111111111111111111111111111111111
            call bitset_l1%flip(0, 63)
            call bitset_l1%to_string(str1)
            print *, str1

            ! 範囲を指定してoldのビット集合のビットを取り出し，newを初期化する．
            ! 00000000000000000000000000000000011111111111111111111111111111111
            call extract(new=bitset_l2, old=bitset_l1, start_pos=32, stop_pos=96)
            call bitset_l2%to_string(str2)
            print *, str2

            ! 文字列からビット集合を初期化．
            call bitset_l1%from_string("1111")

            ! ビット集合のビット数，全てが1かの判別．
            ! 4 T
            print *, bitset_l1%bits(), bitset_l1%all()
        end block
    end subroutine ex_stdlib_bitset
end module ex_stdlib_type
