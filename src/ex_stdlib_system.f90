module ex_stdlib_system
    use, intrinsic :: iso_fortran_env
    use :: stdlib_error
    use :: stdlib_kinds
    use :: stdlib_logger
    use :: stdlib_io
    use :: stdlib_system
    implicit none
    private
    public :: ex_stdlib_error_check
    public :: ex_stdlib_error_error_stop
    public :: ex_stdlib_kinds
    public :: ex_stdlib_logger
    public :: ex_stdlib_io
    public :: ex_stdlib_system_sleep

contains
    !| 論理値を調べてプログラムを停止する手続`check`の使用例．
    subroutine ex_stdlib_error_check()
        implicit none

        block
            ! 引数conditionが`.false.`のとき，"Check failed."を出力し，終了コード1でプログラムを終了する．
            !  Check failed.
            ! ERROR STOP 1
            call check(condition=.false.)
        end block

        block
            integer(int32) :: a = 1
            ! `a==0`が`.false.`のとき，引数msgに渡した文字列を出力し，終了コード1でプログラムを終了する．
            !  error: actual value of a is not equal to expected value 0
            ! ERROR STOP 1
            call check(a == 0, msg="error: actual value of a is not equal to expected value 0")
        end block

        block
            integer(int32) :: a = 2
            ! `a==0`が`.false.`のとき，引数msgに渡した文字列を出力し，終了コード77でプログラムを終了する．
            !  error: actual value of a is not equal to expected value 0
            ! ERROR STOP 77
            call check(a == 0, "error: actual value of a is not equal to expected value 0", code=77)
        end block

        block
            integer(int32) :: a = 3
            ! `a==0`が`.false.`のとき，引数msgに渡した文字列を出力する．
            ! 引数warnに`.true.`を渡すと，プログラムは終了しない．
            !  warning: actual value of a is not equal to expected value 0
            call check(a == 0, "warning: actual value of a is not equal to expected value 0", warn=.true.)
        end block
    end subroutine ex_stdlib_error_check

    !| プログラムの実行を停止する手続`error_stop`の使用例．<br>
    ! Fortran 2008の`error stop`の機能不足を補って，Fortran 2018相当の機能を実現する．
    subroutine ex_stdlib_error_error_stop()
        implicit none

        block
            ! 引数msgに渡した文字列を出力し，非ゼロの終了コードでプログラムを終了する．
            !  Fatal error
            ! ERROR STOP
            call error_stop(msg="Fatal error")
        end block

        block
            ! 引数に渡した文字列を出力し，終了コード999でプログラムを終了する．
            !  Fatal error
            !  ERROR: code          999  was specified.
            ! ERROR STOP
            call error_stop("Fatal error", code=999)
        end block
    end subroutine ex_stdlib_error_error_stop

    !| stdlibで定義されている，変数の種別kindの値の使用例．
    subroutine ex_stdlib_kinds()
        implicit none
        real(sp) :: f_sp
        real(dp) :: f_dp
        logical(lk) :: l
        logical(c_bool) :: l_c
        integer(int8) :: i1
        integer(int16) :: i2
        integer(int32) :: i4
        integer(int64) :: i8

        ! storage size of real(sp) is 32 bits
        ! storage size of real(dp) is 64 bits
        ! storage size of logical(lk) is 32 bits
        ! storage size of logical(c_bool) is 8 bits
        ! storage size of integer(int8) is 8 bits
        ! storage size of integer(int16) is 16 bits
        ! storage size of integer(int32) is 32 bits
        ! storage size of integer(int64) is 64 bits
        print '(3g0)', "storage size of real(sp) is ", storage_size(f_sp), " bits"
        print '(3g0)', "storage size of real(dp) is ", storage_size(f_dp), " bits"
        print '(3g0)', "storage size of logical(lk) is ", storage_size(l), " bits"
        print '(3g0)', "storage size of logical(c_bool) is ", storage_size(l_c), " bits"
        print '(3g0)', "storage size of integer(int8) is ", storage_size(i1), " bits"
        print '(3g0)', "storage size of integer(int16) is ", storage_size(i2), " bits"
        print '(3g0)', "storage size of integer(int32) is ", storage_size(i4), " bits"
        print '(3g0)', "storage size of integer(int64) is ", storage_size(i8), " bits"

        ! 拡張倍精度および4倍精度浮動小数点は，初期は無効化されている．
        print *, xdp, qp
    end subroutine ex_stdlib_kinds

    !| ロガーの使用例．
    subroutine ex_stdlib_logger()
        implicit none

        block
            ! debugログの画面出力．標準設定では出力されない．
            call global_logger%log_debug(message="log message debug")

            ! informationログの画面出力．"タイムスタンプ: INFO: メッセージ"の形式で出力．
            ! 2021-11-21 23:29:31.200: INFO: log message information
            call global_logger%log_information(message="log message information")

            ! warningログの画面出力．"タイムスタンプ: WARN: メッセージ"の形式で出力．
            ! 2021-11-21 23:29:31.201: WARN: log message warning
            call global_logger%log_warning(message="log message warning")

            ! errorログの画面出力．"タイムスタンプ: ERROR: メッセージ"の形式で出力．
            ! 2021-11-21 23:29:31.201: ERROR: log message error
            call global_logger%log_error(message="log message error")
        end block

        block
            ! ロガーを呼び出しているモジュール名と手続名の出力．
            ! "タイムスタンプ: モジュール名 % 手続名: INFO: メッセージ"の形式で出力．
            call global_logger%log_information("log message information", &
                                               module="ex_stdlib_system", &
                                               procedure="ex_stdlib_logger")
        end block

        block
            ! ロガーの設定．debugログを出力する＋メッセージ幅の上限を40文字に設定
            call global_logger%configure(level=debug_level, max_width=40)

            ! 上限を超えないようにスペースで適切に改行
            ! 2021-11-21 23:29:31.202:
            !    ex_stdlib_system %
            !    ex_stdlib_logger: DEBUG: log
            !    message debug
            call global_logger%log_debug(message="log message debug", &
                                         module="ex_stdlib_system", &
                                         procedure="ex_stdlib_logger")

            ! タイムスタンプを表示しない＋メッセージ幅の上限をなしに設定
            call global_logger%configure(time_stamp=.false., max_width=0)

            ! タイムスタンプが出力されない
            ! ex_stdlib_system % ex_stdlib_logger: DEBUG: long long long long log message debug
            call global_logger%log_debug(message="long long long long log message debug", &
                                         module="ex_stdlib_system", &
                                         procedure="ex_stdlib_logger")

            ! ログメッセージを出力する際に空行を挿入＋メッセージが改行されたときの字下げを無効＋メッセージ幅を30文字に設定
            call global_logger%configure(add_blank_line=.true., indent=.false., max_width=30)

            !
            ! ex_stdlib_system %
            ! ex_stdlib_logger: DEBUG: long
            ! long long long log message
            ! debug
            !
            ! ex_stdlib_system %
            ! ex_stdlib_logger: DEBUG: next
            ! log message debug
            call global_logger%log_debug(message="long long long long log message debug", &
                                         module="ex_stdlib_system", &
                                         procedure="ex_stdlib_logger")
            call global_logger%log_debug(message="next log message debug", &
                                         module="ex_stdlib_system", &
                                         procedure="ex_stdlib_logger")
        end block

        block
            use :: stdlib_strings
            integer(int32) :: log_file_unit, stat

            ! 空行を挿入しない＋改行の際に字下げする＋メッセージ幅制限なし＋タイムスタンプを表示するように設定
            call global_logger%configure(add_blank_line=.false., indent=.true., max_width=0, time_stamp=.true.)

            ! ロガーにファイルを追加し，ログメッセージをファイル出力する．
            ! 画面には出力されなくなる．
            call global_logger%add_log_file("log_message.txt", unit=log_file_unit, stat=stat)

            ! ロガーへのファイル追加に成功していれば，その装置番号を表示する．
            ! 画面には表示されず，"log_message.txt"に書き込まれる．
            ! 2021-11-21 23:29:31.203: INFO: log file unit =-10
            if (stat == success) then
                call global_logger%log_information("log file unit ="//to_string(log_file_unit))
            end if

            ! ログファイル"log_message.txt"にメッセージを出力
            ! 2021-11-21 23:29:31.203: DEBUG: log message debug
            ! 2021-11-21 23:29:31.203: INFO: log message information
            ! 2021-11-21 23:29:31.203: WARN: log message warning
            ! 2021-11-21 23:29:31.203: ERROR: log message error
            call global_logger%log_debug("log message debug")
            call global_logger%log_information("log message information")
            call global_logger%log_warning("log message warning")
            call global_logger%log_error("log message error")

            ! ロガーの出力装置に，標準出力を追加．
            ! これ以降，ログファイルと画面の双方にメッセージが出力される
            call global_logger%add_log_unit(unit=output_unit)

            ! ログメッセージの表示．ファイルと画面の両方に同じメッセージが出力される．
            ! 2021-11-21 23:29:31.203: DEBUG: 2nd log message debug
            ! 2021-11-21 23:29:31.203: INFO: 2nd log message information
            ! 2021-11-21 23:29:31.203: WARN: 2nd log message warning
            ! 2021-11-21 23:29:31.204: ERROR: 2nd log message error
            call global_logger%log_debug("2nd log message debug")
            call global_logger%log_information("2nd log message information")
            call global_logger%log_warning("2nd log message warning")
            call global_logger%log_error("2nd log message error")

            ! ロガーに登録されている装置数を表示．
            ! 2021-11-21 23:29:31.204: DEBUG: number of log units = 2
            call global_logger%log_debug("number of log units = "//to_string(global_logger%log_units_assigned()))

            ! ロガーからログファイル"log_message.txt"の装置番号を削除．
            call global_logger%remove_log_unit(unit=log_file_unit, close_unit=.true.)

            ! ログメッセージの表示．これは画面にのみ出力される．
            call global_logger%log_information("3rd log message information")
        end block

        block
            integer(int32) :: log_file_unit
            ! ロガーにファイルを追加．既存のログファイルに付きするように設定．
            call global_logger%add_log_file("log_message.txt", unit=log_file_unit, &
                                            action="write", position="append", status="old")

            ! ログメッセージの表示．このメッセージは画面に出力され，ログファイル"log_message.txt"に追記される．
            ! 2021-11-21 23:29:31.207: INFO: 4th log message information
            call global_logger%log_information("4th log message information")

            ! ロガーからログファイル"log_message.txt"の装置番号を削除．
            call global_logger%remove_log_unit(unit=log_file_unit, close_unit=.true.)

            ! ログファイルの内容
            ! 2021-11-21 23:29:31.203: INFO: log file unit =-10
            ! 2021-11-21 23:29:31.203: DEBUG: log message debug
            ! 2021-11-21 23:29:31.203: INFO: log message information
            ! 2021-11-21 23:29:31.203: WARN: log message warning
            ! 2021-11-21 23:29:31.203: ERROR: log message error
            ! 2021-11-21 23:29:31.203: DEBUG: 2nd log message debug
            ! 2021-11-21 23:29:31.203: INFO: 2nd log message information
            ! 2021-11-21 23:29:31.203: WARN: 2nd log message warning
            ! 2021-11-21 23:29:31.204: ERROR: 2nd log message error
            ! 2021-11-21 23:29:31.204: DEBUG: number of log units = 2
            ! 2021-11-21 23:29:31.207: INFO: 4th log message information
        end block

        block
            integer(int32) :: iostat, io_unit
            character(128) :: iomsg

            ! log_io_errorの動作確認のために存在しないファイルを開く．
            open (newunit=io_unit, file="test.txt", status="old", iostat=iostat, iomsg=iomsg)

            ! ioエラーをログメッセージとして画面に表示．
            ! 2021-11-21 23:29:31.208: I/O ERROR: file test.txt open failure
            ! With iostat = 2
            ! With iomsg = "Cannot open file 'test.txt': No such file or directory"
            call global_logger%log_io_error("file "//"test.txt"//" open failure", iostat=iostat, iomsg=iomsg)
        end block

        block
            ! テキスト内のエラーをログメッセージとして画面に表示する．
            ! 2021-11-21 23:29:31.208
            ! test.txt:0:7
            !
            ! text massage
            !       ^
            ! Error: error summary
            call global_logger%log_text_error(line="text massage", &
                                              column=7, &
                                              summary="error summary", &
                                              filename="test.txt", &
                                              line_number=0, &
                                              caret="^")
        end block

        block
            ! 新しいロガーを宣言．
            type(logger_type) :: new_logger

            ! new_loggerは出力レベルをwarning以上にしたので，informationは表示されない．
            ! 2021-11-23 20:00:02.315: INFO: global logger
            ! 2021-11-23 20:00:02.316: WARN: global logger
            ! 2021-11-23 20:00:02.317: WARN: user logger
            call new_logger%configure(level=warning_level)
            call global_logger%log_information("global logger")
            call new_logger%log_information("user logger")
            call global_logger%log_warning("global logger")
            call new_logger%log_warning("user logger")
        end block
    end subroutine ex_stdlib_logger

    !| stdlib_ioで定義されている手続`open`, `savetxt`, `loadtxt`の使用例．
    subroutine ex_stdlib_io()
        implicit none

        block
            integer(int32) :: unit
            character(128) :: str

            ! open文を簡略化するopen関数．
            ! 書き込み＋テキストモードで開く．
            unit = open ("example_io.txt", "wt")

            write (unit, '(A)') "example of stdlib_io%open"
            close (unit)

            ! 読み取り＋テキストモードで開く
            unit = open ("example_io.txt", "rt")
            read (unit, '(A)') str
            print *, str

            ! 書き込みをしようとするとエラー．
            ! Fortran runtime error: Cannot write to file opened for READ
            ! write (unit, '(A)') "append to example_io.txt"
            close (unit)

            ! 追記モードで開く
            unit = open ("example_io.txt", "a")
            write (unit, '(A)') "append to example_io.txt"
            close (unit)
        end block

        block
            use :: stdlib_math
            use :: iso_c_binding

            real(real32) :: array(16, 2)

            enum, bind(c)
                enumerator :: x = 1
                enumerator :: f
            end enum

            ! stdlib_mathで定義されているlinspaceを利用してx = [0, 2π]を設定し，sin(x)を計算．
            array(:, x) = linspace(0., 2.*acos(-1.), 16)
            array(:, f) = sin(array(:, x))

            ! 2次元配列をテキストモードで出力するsavetxt手続．
            !       j      1                2         i
            ! array = 0.00000000E+00  0.00000000E+00  1
            !         4.18879032E-01  4.06736642E-01  2
            !         8.37758064E-01  7.43144870E-01  3
            !         1.25663710E+00  9.51056540E-01
            !         1.67551613E+00  9.94521916E-01
            !         2.09439516E+00  8.66025388E-01
            !         2.51327419E+00  5.87785184E-01
            !         2.93215322E+00  2.07911611E-01
            !         3.35103226E+00 -2.07911789E-01
            !         3.76991129E+00 -5.87785363E-01
            !         4.18879032E+00 -8.66025448E-01
            !         4.60766935E+00 -9.94521916E-01
            !         5.02654839E+00 -9.51056480E-01
            !         5.44542742E+00 -7.43144751E-01
            !         5.86430645E+00 -4.06736493E-01
            !         6.28318548E+00  1.74845553E-07
            call savetxt("sin.txt", array)
        end block

        block
            real(real32), allocatable :: array(:, :)

            ! テキストファイルから2次元配列を読み取るloadtxt手続．
            call loadtxt("sin.txt", array)
            print *, array
        end block
    end subroutine ex_stdlib_io

    !| stdlib_systemで定義されている手続`sleep`の使用例．
    subroutine ex_stdlib_system_sleep()
        implicit none
        integer(int32) :: count_start_c, count_end_c
        integer(int32) :: count_per_sec

        call system_clock(count_start_c, count_per_sec)

        ! ミリ秒単位でプログラムの実行を中断
        call sleep(millisec=100)

        call system_clock(count_end_c, count_per_sec)

        ! sleep 0.108999997sec
        print '(3g0)', "sleep ", real(count_end_c - count_start_c)/real(count_per_sec), "sec"
    end subroutine ex_stdlib_system_sleep
end module ex_stdlib_system
