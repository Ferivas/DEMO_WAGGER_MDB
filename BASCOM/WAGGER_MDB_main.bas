'WAGGER_LPR_main.bas
'
'                 WATCHING Soluciones Tecnológicas
'                    Fernando Vásquez - 25.06.15
'
' Programa para almacenar los datos que se adquieren de cuatro canales del
' WAGGER
'
' ENABUG.N hablitaciones
' ENABUG.0=1 indica el numero de muestras de lso canales
' ENABUG.1=1 indica el promedio conversor ADC interno para Vmain y Vmdc
' ENABUG.2=1 indica valores instantaneos de canales WAGGER
' ENABUG.3=1 indica valores promedio de canales WAGGER


$version 0 , 1 , 389
$regfile = "m1284pdef.dat"
'$crystal = 11059200
$crystal = 7372800
$baud = 115200
$baud1 = 9600


$hwstack = 300
$swstack = 300
$framesize = 300

$projecttime = 291


'Declaracion de constantes
'  Permite configurar por defecto el servidor , puerto y APN, según se tenga pruebas de laboratorio
Const Modoproduc = 1                                        '0 LABORATORIO 1 PRODUCCION

'RTC
Const Ds3231r = &B11010001                                  'DS3231 is very similar to DS1307 but it include a precise crystal
Const Ds3231w = &B11010000
'ADC
Const Numadc = 2                                            'Vmain y Vmdc
Const Numadc_masuno = Numadc + 1
Const Numsample = 4
Const Tsample = 1

Const Numcan = 4
Const Numcan_masuno = Numcan + 1

Const Numtxaut = 4
Const Numtxaut_masuno = Numtxaut + 1

Const Numtlf = 4
Const Numtlf_masuno = Numtlf + 1

Const Nbuf = 64

'Configuracion de entradas/salidas
Led1 Alias Portc.3                                          'LED ROJO
Config Led1 = Output

Pwrsd Alias Portc.4
Config Pwrsd = Output
Reset Pwrsd

'Pwrmdc Alias Portb.0
'Config Pwrmdc = Output
'Set Pwrmdc

Rstmdc Alias Portb.0
Reset Rstmdc
Config Rstmdc = Output

'Configuración de Interrupciones
'TIMER0
Config Timer0 = Timer , Prescale = 1024                     'Ints a 100Hz si Timer0=184
On Timer0 Int_timer0
Enable Timer0
Start Timer0

'Config Timer1 = Timer , Prescale = 256                      'Ints a 100Hz si Timer0=184
'On Timer1 Int_timer1
'Enable Timer1
'Start Timer1

Config Timer2 = Timer , Prescale = 1024                     'Ints a 100Hz si Timer0=184
On Timer2 Int_timer2
Enable Timer2
Start Timer2



Dim Dummy As Byte
Config Date = Dmy , Separator = /
Config Clock = User
'Config Clock = Soft , Gosub = Sectic


   'ADC
Config Adc = Single , Prescaler = Auto                      ', Reference = Internal
Enable Adc
Start Adc

Pwradc Alias Portc.5
Config Pwradc = Output
Reset Pwradc

Sela Alias Portb.1
Config Sela = Output

Selb Alias Portb.2
Config Selb = Output

Ena Alias Portb.3
Config Ena = Output

Sdin Alias Pind.4
Config Sdin = Input

Sck Alias Portd.5
Config Sck = Output

Conv Alias Portd.6
Config Conv = Output

Pwrsta Alias Pina.2
Config Pwrsta = Input
'Set Pwrsta

Cts Alias Pina.7
Config Cts = Input

Rts Alias Portd.7
Config Rts = Output

'Pinbug Alias Porta.4
'Config Pinbug = Output

' Puerto serial 1
Open "com1:" For Binary As #1
On Urxc At_ser1
Enable Urxc

On Urxc1 At_ser2
Enable Urxc1
Open "com2:" For Binary As #2


Config Sda = Portc.1
Config Scl = Portc.0
Config I2cdelay = 20
I2cinit
'Set Portc.1
'Set Portc.0

Enable Interrupts


'*******************************************************************************
'* Archivos incluidos
'*******************************************************************************
Print #1 , "WAGGER_LPR"
$include "Config_MMCSD_HC_M1284p.bas"
If Gbdriveerror = 0 Then
   $include "Config_AVR-DOS.bas"
End If
$include "WAGGER_MDB_archivos.bas"

'Programa principal

Call Vercfg()

Tmpb = 0

Do
   Incr Tmpb
   Print #1 , "Int. Leer RTC " ; Tmpb
   Call Getdatetimeds3231()
Loop Until Errorrtc = 0 Or Tmpb = 5

Do
   Call Diskinsertion()
   If Sdinitok = 0 Then
      Wait 1
      Incr Tmpb
      'Reset Watchdog
   End If
Loop Until Sdinitok = 1 Or Tmpb = 3


If Errorrtc = 0 Then
   Print #1 , "Hora=" ; Time$ ; ",Fecha=" ; Date$
   Estado_led = 1
Else
   Print # 1 , "ERROR CLK"
   Estado_led = 2
End If

Call Inivar()
Call Pwrinpset()

Reset Initxinicio

Call Diskinsertion()
Chdir "\"
Tmpdir = "CONFIG"
Chdir Tmpdir
Filenotx = "CONFIG.TXT"
Call Savetxini()
Chdir "\"
Tmpdir = "NOTX"
Chdir Tmpdir
Filenotx = "NO_INI.TXT"
Call Savetxini()



Do

   If Newsec = 1 Then
      Reset Newsec
      Tmpstr8 = Date$
      Namelog = Mid(tmpstr8 , 7 , 2) + Mid(tmpstr8 , 4 , 2) + "_LOG.TXT"
      If Namelog <> Namelogant Then
         Print #1 , "FILELOG <" ; Namelog ; ">"
         Namelogant = Namelog
         Filelog = Namelog
         Filelogeep = Namelog
      End If
    End If

   If Sernew = 1 Then                                       'DATOS SERIAL 1
      Reset Sernew
      'Print #1 , "SER1=" ; Serproc
      Call Procser()
   End If

   If Sernew1 = 1 Then                                      'DATOS SERIAL 1
      Reset Sernew1
      Print #1 , "MDC=" ; Serproc1
      'Serproc = Serproc1
      Call Procmdc()
   End If

   If Inilog = 1 Then
      Reset Inilog
      Call Printlogsd()
   End If

   If Iniadc = 1 Then
      Reset Iniadc
      Call Leeradc()                                        ' Lee Vmain, Vmdc
      'Call Leerinadc()                                      ' Lee los canales del LTC1864
   End If

   If Iniadq = 1 Then
      Reset Iniadq
      Call Leerinadc()                                      ' Lee los canales del LTC1864
   End If

   If Inivariables = 1 Then
      Reset Inivariables
      Call Inivar()
   End If

   If Pwrsta <> Pwrstaant Then
      If Enabug.4 = 1 Then
         Print #1 , "PWRSTA=" ; Pwrsta
      End If
      Pwrstaant = Pwrsta
      If Pwrsta = 1 Then
         Set Rts
         Mopstr = "AC"
      End If
   End If

   If Pwrsta = 1 Then
      Cntrpwrsta = 0
   Else
      Incr Cntrpwrsta
      If Cntrpwrsta > 10 Then
         Cntrpwrsta = 0
         Reset Rts
         Mopstr = "BAT"
      End If
   End If

   If Rts <> Rtsant Then
      Print #1 , "RTS=" ; Rts
      Rtsant = Rts
   End If

   Incr Cntrauto
   Cntrauto = Cntrauto Mod Numtxaut
   If Iniauto.cntrauto = 1 Then
      Tmpb = Cntrauto + 1
      Print #1 , "TXauto " ; Tmpb ; Time$
      Print #1 , "TEST"
      Reset Iniauto.cntrauto
   End If

   If Initac = 1 And Samplerdy = 1 Then
      Reset Initac
      Call Savetx()
   End If

   If Newtrytx = 1 And Waitrx = 0 Then
     Ptrtx = Ptrtx Mod Nbuf
      Incr Ptrtx
      If Txbuf(ptrtx) = 1 Then
         Reset Newtrytx
         'Print #1 , "PtrTX=" ; Ptrtx
         If Cts = 1 Then
            Call Txaut()
            Set Iniwaitrx
            Set Waitrx
            Reset Flagfile
            Idrxtrama = "NO"
         Else
            Kw = Ptrtx
            Call Gentrama()
            Tramatmp = Tmpstr52 + "R"
            Call Writesd(filenotx , Tramatmp , "NOTX")
            Tmpl = Filelen(filenotx)
            If Tmpl > 900 Then
               Incr Ptrnotx
               Ptrnotxeep = Ptrnotx
               Filenotx = "NO_" + Str(ptrnotx) + ".TXT"
            End If
            Txbuf(ptrtx) = 0
         End If
      End If
   End If

   If Iniprg = 1 Then
      Print #1 , "Entra modo PROG"
      Estado_led = 12
      Do
         If Sernew = 1 Then                                       'DATOS SERIAL 1
            Reset Sernew
            Print #1 , "SER1=" ; Serproc
            Call Procser()
         End If
         If Inilog = 1 Then
            Reset Inilog
            Call Printlogsd()
         End If
      Loop Until Iniprg = 0
      Estado_led = 1
      Print #1 , "FIN modo PROG"
   End If

   If Initxinicio = 1 Then
      Reset Initxinicio
      Chdir "\"
      Tmpdir = "NOTX"
      Chdir Tmpdir
      Filenotx = "NO_INI.TXT"
      Call Savetxini()
      Chdir "\"
      Tmpdir = "CONFIG"
      Chdir Tmpdir
      Filenotx = "CONFIG.TXT"
      Call Savetxini()
   End If

   If Inirtx = 1 Then
      Reset Inirtx
      Call Versd()
      Print #1 , "RE TX PER " ; Time$ ; "," ; Date$
      Chdir "\"
      Chdir "NOTX"                                       ' Para ir al directorio raiz
      Dirstr = Dir( "NO*.*")
      If Len(dirstr) > 0 Then
            Tmpstr8 = Filedatetime(dirstr)
            Print #1 , Dirstr ; "," ; Filelen(dirstr) ; "," ; Tmpstr8
            Reset Watchdog
            'Dirstr = Dir()                      $
            Filetmp = Dirstr
            Set Initxfile
      End If
   End If

   If Initxfile = 1 And Waitrx = 0 Then
      Reset Initxfile
      Print #1 , "NewTX FILE <" ; Filetmp ; ">"
      If Cts = 1 Then
         Call Versd()
         Chdir "\"
         Chdir "NOTX"
         Dirstr = Dir(filetmp)
         If Len(dirstr) > 0 Then
            Call Diskinsertion()
            If Sdinitok = 1 Then
               Chdir "\"
               Chdir "NOTX"
              Ff = Freefile()
              Open Filetmp For Binary As #ff
              Tmpl3 = Lof(#ff)
              Print #1 , "FILE <" ; Filetmp ; ">, LOF=" ; Tmpl3
              Idtxfile = Filetmp
              Atsnd = "$INIBUF," + Idtxfile
              Print #1 , Atsnd
              Print #2 , Atsnd
              Do
                 Atsnd = ""
                 Line Input #ff , Atsnd
                 Print #1 , Atsnd
                 Print #2 , Atsnd
              Loop Until Eof(#ff) <> 0
              Close #ff
              Atsnd = "$FINBUF"
              Print #1 , Atsnd
              Print #2 , Atsnd
              Set Iniwaitrx
              Set Waitrx
              Set Flagfile
              Idrxfile = "NO"
            Else
               Print #1 , "ERROR SD"
            End If
         End If
      Else
         Print #1 , "MDC no ready"
      End If

   End If

   If Proctramfile = 1 Then
      Reset Proctramfile
      If Flagfile = 1 Then
         Print #1 , "TXfile=" ; Idtxfile
         Print #1 , "RXfile=" ; Idrxfile
         If Idtxfile = Idrxfile Then
            If Txtrama = "OK" Then
               Call Versd()
               Filetmp = Idtxfile
               Tmpstr52 = Filetmp
               Mid(tmpstr52 , 1 , 2) = "SI"
               Print #1 , Tmpstr52
               Print #1 , "Verifico que no haya archivo SI_*.txt en SD"
               Chdir "\"
               Chdir "NOTX"
               Dirstr = Dir(tmpstr52)
               If Len(dirstr) > 0 Then
                  Print #1 , "Borro archivo " ; Tmpstr52
                  Chdir "\"
                  Chdir "NOTX"
                  Ff = Freefile()
                  Open Tmpstr52 For Binary As #ff
                  Close #ff
                  Kill Tmpstr52
                  Print #1 , "Err code:" ; Gbdoserror
                  Flush #ff
                  Atsnd = "Borro archivo " + Tmpstr52
                  Call Writesd( "EV_LOG.TXT" , Atsnd , "LOG")
               End If
               Name Filetmp As Tmpstr52
               Chdir "\"
               Chdir "NOTX"
               Dirstr = Dir(filetmp)
               If Len(dirstr) > 0 Then
                  Print #1 , "FILE <" ; Filetmp ; "> NO BORRADO"
                  Chdir "\"
                  Chdir "NOTX"
                  Ff = Freefile()
                  Open Filetmp For Binary As #ff
                  Close #ff
                  Kill Filetmp
                  Print #1 , "Err code:" ; Gbdoserror
                  Flush #ff
                  'Ser1snd = "Borro archivo " + Filetmp
               Else
                  Print #1 , "FILE <" ; Filetmp ; "> borrado"
               End If
               Atsnd = "TX OK," + Filetmp
            Else
               Print #1 , "NO TX file;" ; Filetmp
               Atsnd = "ERR Tx," + Filetmp
            End If
            Atsnd = Atsnd + "," + Time$ + "," + Date$
            Call Writesd( "EV_LOG.TXT" , Atsnd , "LOG")
         Else
            Print #1 , "NO ID file iguales"
         End If
      Else
         Print #1 , "TXtrama=" ; Idtxtrama
         Print #1 , "RXtrama=" ; Idrxtrama
         If Idtxtrama = Idrxtrama Then
            If Txtrama = "OK" Then
               Print #1 , "Tx trama OK"
            Else
               Tramatmp = Tramatmp + "R"
               Call Writesd(filenotx , Tramatmp , "NOTX")
               Tmpl = Filelen(filenotx)
               If Tmpl > 900 Then
                  Incr Ptrnotx
                  Ptrnotxeep = Ptrnotx
                  Filenotx = "NO_" + Str(ptrnotx) + ".TXT"
               End If
            End If
         Else
            Print #1 , "NO ID trama iguales"
            Tramatmp = Tramatmp + "R"
            Call Writesd(filenotx , Tramatmp , "NOTX")
            Tmpl = Filelen(filenotx)
            If Tmpl > 900 Then
               Incr Ptrnotx
               Ptrnotxeep = Ptrnotx
               Filenotx = "NO_" + Str(ptrnotx) + ".TXT"
            End If
         End If
      End If
      Reset Waitrx
   End If

   If Initstadc = 1 Then
      Do
         If Sernew = 1 Then                                       'DATOS SERIAL 1
            Reset Sernew
            Print #1 , "SER1=" ; Serproc
            Call Procser()
         End If
         If Iniadc = 1 Then
            Reset Iniadc
            Call Leercanal(canaltst)
         End If
      Loop Until Initstadc = 0
      Print #1 , "FIN TST ADC"
   End If

   If Cts <> Ctsant Then
      Ctsant = Cts
      Print #1 , "CTS=" ; Cts
   End If

   If Pwrsave = 1 Then
      'Reset Pwrsave
      Set Inipwrsave
      Set Pwrsd
      Set Pwradc
      Stop Adc
      Disable Adc
      Print #1 , "SLEEP en " ; Time$ ; "," ; Date$
      Set Led1
      Disable Timer0
      Disable Urxc
      Disable Urxc1
      Disable Aci
      Disable Spi
      Do
         Config Powermode = Powersave
         'NOP
         'NOP
         'Reset Watchdog
      Loop Until Pwrsave = 0
      Enable Urxc
      Enable Urxc1
      Enable Timer0
      Print #1 , "Fin sleep" ; Time$ ; "," ; Date$
      Reset Pwradc
      Reset Pwrsd
      Enable Adc
      Enable Spi
      Start Adc
      Call Versd()
   End If


Loop