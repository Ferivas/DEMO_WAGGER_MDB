'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*  SD_Archivos.bas                                                        *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                                                                             *
'*  Variables, Subrutinas y Funciones                                          *
'* WATCHING SOLUCIONES TECNOLOGICAS                                            *
'* 25.06.2015                                                                  *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

$nocompile

$projecttime = 587
'*******************************************************************************
'Declaracion de subrutinas
'*******************************************************************************
Declare Sub Inivar()
Declare Sub Vercfg()
Declare Sub Procser()
Declare Sub Diskinsertion()
Declare Sub Versd()
'RTC
Declare Sub Getdatetimeds3231()
Declare Sub Error(byval Genre As Byte)
Declare Sub Setdateds3231()
Declare Sub Settimeds3231()

'SD
Declare Sub Verdir()
Declare Sub Writesd(byval Archivo As String , Texto As String * 255 , Byval Directorio As String * 12)
Declare Sub Printlogsd()
'ADC
Declare Sub Leeradc()
Declare Sub Leercanal(byval Canal As Byte)
Declare Sub Leerinadc()
Declare Sub Defaultvalues()
Declare Sub Leer1864()
Declare Sub Savetxini()
'Declare Sub Txoper()
Declare Sub Savetx()
Declare Sub Txaut()
Declare Sub Procmdc()
Declare Sub Gentrama()
Declare Sub Pwrinpset()

'*******************************************************************************
'Declaracion de variables
'*******************************************************************************
Dim Tmpb As Byte , Tmpb2 As Byte , Tmpb3 As Byte , Tmpb4 As Byte
Dim Tmpw As Word
Dim Tmpl As Long , Tmpl2 As Long , Tmpl3 As Long , Lsyssec As Long
Dim Tmplisr As Long
Dim Tmps As Single
Dim Tmpbit As Bit
Dim Iniprg As Bit
Dim Initxinicio As Bit
Dim Codcmd As String * 5
Dim Flagcmdsnd As Bit

Dim Cmdtmp As String * 6
Dim Atsnd As String * 255
Dim Cmderr As Byte
Dim Tmpstr8 As String * 16
Dim Tmpstr52 As String * 255

'SD
Dim Sdinitok As Bit
Dim Debugsd As Byte
Dim Dirstr As String * 16
Dim Inilog As Bit
Dim Tmpdir As String * 16
Dim Ff As Byte
Dim Ptrsd As Long
Dim Ptrsd1 As Long

Dim Cfgokeep As Eram Byte
Dim Cfgok As Byte


'Variables TIMER0
Dim T0c As Byte
Dim Num_ventana As Byte
Dim Estado As Long
Dim Estado_led As Byte
Dim Iluminar As Bit

' TIMER2
Dim Lsecday As Long
Dim T2 As Byte
Dim T21 As Byte
Dim T2c As Byte
Dim Timestr As String * 10

'RTC
Dim Dow As Byte
Dim Errorrtc As Byte
Dim Horamin As Long
Dim Horamineep As Eram Long
Dim Horatmp As String * 10
Dim Fechatmp As String * 10
Dim Newsec As Bit
Dim Timebuf(3) As Byte
Dim Datebuf(3) As Byte

'MODBUS
Dim Addrmdb As Word
Dim Valmdb As Word

'ADC
Dim Adcn As Byte , Adct As Byte
Dim Cntrsmpl As Byte
Dim Tmpwadc As Word
Dim Adccntri(numadc) As Single
Dim Iniadc As Bit
Dim Adc_data(numadc) As Single
Dim Kadc(numadc) As Single
Dim Kadceep(numadc) As Eram Single
Dim Smplrdy As Bit
Dim Vmain As Single
Dim Vmdc As Single
Dim Cntradc As Byte

Dim Pwrstaant As Bit
Dim Cntrpwrsta As Byte
Dim Rtsant As Bit

Dim Enabug As Byte
Dim Enabugeep As Eram Byte

Dim Inivariables As Bit
Dim Cntrini As Word
Dim Cntrinieep As Eram Word

'Var Canales
Dim Valwadc(numcan) As Word
Dim Enacan As Byte                                          'Habilitacion de canales ADC
Dim Samplerdy As Bit
Dim Enacaneep As Eram Byte
Dim Idcan(Numcan) As String * 8                             'ID Canal
Dim Idcaneep(numcan) As Eram String * 8
Dim Iniescala(Numcan) As Single                             'Inicio de escala
Dim Iniescalaeep(Numcan) As Eram Single
Dim Fonescala(Numcan) As Single                             'Fondo de escala
Dim Fonescalaeep(Numcan) As Eram Single
Dim Mval(numcan) As Single
Dim Bval(numcan) As Single
Dim Ndcan(numcan) As Byte
Dim Ndcaneep(numcan) As Eram Byte
Dim Tipocan(numcan) As String * 6
Dim Tipocaneep(numcan) As Eram String * 6
Dim Unican(numcan) As String * 6
Dim Unicaneep(numcan) As Eram String * 6

Dim Tmpfusing As String * 32
Dim Initstadc As Bit
Dim Canaltst As Byte

Dim Cntrsamples As Word
Dim Numsamples As Single
Dim Numsampleb As Word
Dim Can_val(numcan) As Long
Dim Can_vals(numcan) As Single
Dim Kcal(numcan) As Single
Dim Kcaleep(numcan) As Eram Single

Dim Tadq As Word                                            'Tiempo de muestreo de datos de 1 a 65536 segundos
Dim Tadqeep As Eram Word
Dim Iniadq As Bit

Dim Tac As Word
Dim Taceep As Eram Word
Dim Initac As Bit

Dim Txdattime(nbuf) As Long                                 'Hora, Fecha
Dim Txdatcan1(nbuf) As Single , Txdatcan2(nbuf) As Single
Dim Txdatcan3(nbuf) As Single , Txdatcan4(nbuf) As Single
Dim Txdatbat(nbuf) As Single                                'Batería
Dim Txdatvmdc(nbuf) As Single                               'Batería
'Dim Txdatcntrini(nbuf) As Word                              'Contrini
Dim Gsmdat(nbuf) As Byte
Dim Txbuf(nbuf) As Byte

Dim Ptrsavedat As Byte
Dim Kw As Byte
Dim Gsmval As String * 6
Dim Newtrytx As Bit
Dim Ptrtx As Byte

Dim Idest As String * 64
Dim Idesteep As Eram String * 64

Dim Autoval(numtxaut) As Long                               'Tiempo entre tx periodicas
Dim Autovaleep(numtxaut) As Eram Long
Dim Iniauto As Byte

Dim Enaauto As Byte
Dim Enaautoeep As Eram Byte
Dim Cntrauto As Byte

Dim Mop As Bit
Dim Mopstr As String * 3
Dim Cntrtramas As Word
Dim Cntrtramaseep As Eram Word
Dim Imei As String * 18
Dim Imeieep As Eram String * 18
Dim Numserialwtch As String * 33
Dim Numserialwtcheep As Eram String * 33
Dim Idserial As String * 33
Dim Idserialeep As Eram String * 33

' Variables manejo archivos
Dim Ptrnotx As Word
Dim Ptrnotxeep As Eram Word
Dim Iptx As String * 48
Dim Iptxeep As Eram String * 48
Dim Ptotx As String * 6
Dim Ptotxeep As Eram String * 6
Dim Apn As String * 32
Dim Apneep As Eram String * 32
Dim Filenotx As String * 16
Dim Filetmp As String * 16
Dim Initxfile As Bit

'TLFS
Dim Tlf(numtlf) As String * 20
Dim Tlfeep(numtlf) As Eram String * 20

'RTX
Dim Ctsant As Bit
Dim Inirtx As Bit
Dim Trtx As Word
Dim Trtxeep As Eram Word

'PROC MDC
Dim Newcmdmdc As Bit
Dim Fonorxmdc As String * 32
Dim Fecharxmdc As String * 32
Dim Namelog As String * 12
Dim Namelogant As String * 12
Dim Filelog As String * 16
Dim Filelogeep As Eram String * 16
Dim Idtxtrama As String * 18
Dim Idtxfile As String * 18
Dim Idrxtrama As String * 18
Dim Idrxfile As String * 18
Dim Txtrama As String * 6
Dim Tramatmp As String * 255

Dim Waitrx As Bit                                           'Confirmacion de recepcion
Dim Iniwaitrx As Bit
Dim Cntrwaitrx As Byte
Dim Proctramfile As Bit
Dim Flagfile As Bit
Dim Pwrsave As Bit
Dim Inipwrsave As Bit
Dim Cntrpwrsave As Byte

'COORDENADAS
Dim Estadogps As String * 1
Dim Estadogpseep As Eram String * 1
Dim Latitud As String * 11
Dim Latitudeep As Eram String * 11
Dim Longitud As String * 12
Dim Longitudeep As Eram String * 12

'Variables SERIAL0
Dim Ser_ini As Bit , Sernew As Bit
Dim Ser_ini1 As Bit , Sernew1 As Bit
Dim Serdata1 As String * 200 , Serrx1 As Byte , Serproc1 As String * 200
Dim Numpar As Byte
Dim Horasplit(4) As String * 3
Dim Regmdb(6)as String * 16
Dim Cmdsplit(34) As String * 100
Dim Serdata As String * 200 , Serrx As Byte , Serproc As String * 200



'*******************************************************************************
'* END public part                                                             *
'*******************************************************************************


Goto Loaded_arch

'*******************************************************************************
' INTERRUPCIONES
'*******************************************************************************

'*******************************************************************************
' Subrutina interrupcion de puerto serial 1
'*******************************************************************************
At_ser1:
   Serrx = Udr

   Select Case Serrx
      Case "$":
         Ser_ini = 1
         Serdata = ""

      Case 13:
         If Ser_ini = 1 Then
            Ser_ini = 0
            Serdata = Serdata + Chr(0)
            Serproc = Serdata
            Sernew = 1
            Enable Timer0
         End If

      Case Is > 31
         If Ser_ini = 1 Then
            Serdata = Serdata + Chr(serrx)
         End If

   End Select

Return


Return

'*******************************************************************************
' Subrutina interrupcion de puerto serial 2
'*******************************************************************************
'*******************************************************************************
' Subrutina interrupcion de puerto serial 2
'*******************************************************************************
At_ser2:
   Serrx1 = Udr1
   Select Case Serrx1
      Case "%":
         Ser_ini1 = 1
         Serdata1 = ""

      Case 13:
         If Ser_ini1 = 1 Then
            Ser_ini1 = 0
            Serdata1 = Serdata1 + Chr(0)
            Serproc1 = Serdata1
            Sernew1 = 1
         End If

      Case Is > 31
         If Ser_ini1 = 1 Then
            Serdata1 = Serdata1 + Chr(serrx1)
            If Len(serdata1) > 160 Then
               Serdata1 = ""
            End If

         End If
   End Select
Return

'*******************************************************************************



'*******************************************************************************
' TIMER0
'*******************************************************************************
Int_timer0:
   Timer0 = 184
   Incr T0c
   T0c = T0c Mod 8
   If T0c = 0 Then
      Num_ventana = Num_ventana Mod 32
      Estado = Lookup(estado_led , Tabla_estado)
      Iluminar = Estado.num_ventana
      Toggle Iluminar
      Led1 = Iluminar
      Incr Num_ventana
   End If

Return

Int_timer2:
   'Timer1 = &H8F80                                          'Ints cada segundo
   Timer2 = 184
   Incr T2c
   T2c = T2c Mod 100
   If T2c = 0 Then
      Lsyssec = Syssec(time$ , Date$)
      Incr Lsyssec
      Time$ = Time(lsyssec)
      'Timestr = Time$
      Date$ = Date(lsyssec)
      Set Newsec
      Incr Cntradc
      Cntradc = Cntradc Mod Tsample
      If Cntradc = 0 Then
         Set Iniadc
      End If
      Lsecday = Secofday()
      For T2 = 1 To Numtxaut
         T21 = T2 - 1
         If Enaauto.t21 = 1 Then
            If Autoval(t2) = Lsecday Then
               Set Iniauto.t21
            End If
         End If
      Next
      Lsyssec = Syssec()
      Tmplisr = Lsyssec Mod Tac
      If Tmplisr = 0 Then Set Initac
      Tmplisr = Lsyssec Mod Tadq
      If Tmplisr = 0 Then Set Iniadq

      Tmplisr = Lsyssec Mod Trtx
      If Tmplisr = 0 Then Set Inirtx

      Set Newtrytx
      If Iniwaitrx = 1 Then
         Incr Cntrwaitrx
         Cntrwaitrx = Cntrwaitrx Mod 120                       'Espero 2 minutos confirmacion de rx
         If Cntrwaitrx = 0 Then
            Reset Iniwaitrx
            Set Proctramfile
         End If
      End If

      If Inipwrsave = 1 Then
         Incr Cntrpwrsave
         Cntrpwrsave = Cntrpwrsave Mod 10
         If Cntrpwrsave = 0 Then
           Reset Inipwrsave
           Reset Pwrsave
         End If

      End If
   End If


Return


'*******************************************************************************
' TIMER2
'*******************************************************************************
Sectic:
   Set Newsec
   Incr Cntradc
   Cntradc = Cntradc Mod Tsample
   If Cntradc = 0 Then
      Set Iniadc
   End If
   Lsecday = Secofday()
   For T2 = 1 To Numtxaut
      T21 = T2 - 1
      If Enaauto.t21 = 1 Then
         If Autoval(t2) = Lsecday Then
            Set Iniauto.t21
         End If
      End If
   Next
   Lsyssec = Syssec()
   Tmplisr = Lsyssec Mod Tac
   If Tmplisr = 0 Then Set Initac
   Tmplisr = Lsyssec Mod Tadq
   If Tmplisr = 0 Then Set Iniadq

   Tmplisr = Lsyssec Mod Trtx
   If Tmplisr = 0 Then Set Inirtx

   Set Newtrytx
   If Iniwaitrx = 1 Then
      Incr Cntrwaitrx
      Cntrwaitrx = Cntrwaitrx Mod 120                       'Espero 2 minutos confirmacion de rx
      If Cntrwaitrx = 0 Then
         Reset Iniwaitrx
         Set Proctramfile
      End If
   End If

   If Inipwrsave = 1 Then
      Incr Cntrpwrsave
      Cntrpwrsave = Cntrpwrsave Mod 10
      If Cntrpwrsave = 0 Then
        Reset Inipwrsave
        Reset Pwrsave
      End If

   End If

Return

Getdatetime:
   'Toggle Pinbug
Return

Setdate:
Return

Settime:
Return



'*******************************************************************************
' SUBRUTINAS
'*******************************************************************************

Sub Vercfg()
   Print #1 , "Verificar Config. Inicial"
   Cfgok = Cfgokeep
   Tmpb2 = 0
   Estado_led = 3
   Do
      If Sernew = 1 Then                                       'DATOS SERIAL 1
         Reset Sernew
         Print #1 , "SER1=" ; Serproc
         Call Procser()
      End If
      If Newsec = 1 Then
         Reset Newsec
         Incr Tmpb
         Incr Tmpb2
         Tmpb = Tmpb Mod 10
         If Tmpb = 0 Then
            Print #1 , "Ingrese valores de configuracion"
         End If
      End If
   Loop Until Cfgok = 1                                     'Cfgok = 1
   Print #1 , "CONFIG. INICIAL OK"
   Estado_led = 2

End Sub

'*******************************************************************************
' Inicialización de variables
'*******************************************************************************
Sub Inivar()

Reset Led1
'dummy=0
'Print #1 , "************ DRIVER AUDIO ************"
Print #1 , Version(1)
Print #1 , Version(2)
Print #1 , Version(3)
'Estado_led = 1
Debugsd = 0
Enabug = Enabugeep
Print #1 , "ENABUG=" ; Enabug
For Tmpb = 1 To Numadc
    Kadc(tmpb) = Kadceep(tmpb)
    Print #1 , Tmpb ; "," ; Kadc(tmpb)
Next
Reset Smplrdy
Cntrini = Cntrinieep
Incr Cntrini
Cntrinieep = Cntrini
Print #1 , "Cntrini=" ; Cntrini
Enacan = Enacaneep
Print #1 , "ENAADC=" ; Bin(enacan)

For Tmpb = 1 To Numcan
   Idcan(tmpb) = Idcaneep(tmpb)
   Iniescala(tmpb) = Iniescalaeep(tmpb)
   Fonescala(tmpb) = Fonescalaeep(tmpb)
   Print #1 , "IDcan(" ; Tmpb ; ")=" ; Idcan(tmpb)
   Print #1 , "INIesc(" ; Tmpb ; ")=" ; Iniescala(tmpb)
   Print #1 , "FONesc(" ; Tmpb ; ")=" ; Fonescala(tmpb)
   Mval(tmpb) = Fonescala(tmpb) - Iniescala(tmpb)
   Mval(tmpb) = Mval(tmpb) / 52428
   Bval(tmpb) = 5 * Iniescala(tmpb)
   Bval(tmpb) = Bval(tmpb) - Fonescala(tmpb)
   Bval(tmpb) = Bval(tmpb) / 4
   Print #1 , "Mval(" ; Tmpb ; ")=" ; Mval(tmpb)
   Print #1 , "Bval(" ; Tmpb ; ")=" ; Bval(tmpb)
   Kcal(tmpb) = Kcaleep(tmpb)
   Print #1 , "Kcal(" ; Tmpb ; ")=" ; Kcal(tmpb)
   Ndcan(tmpb) = Ndcaneep(tmpb)
   Print #1 , "Num. dec(" ; Tmpb ; ")=" ; Ndcan(tmpb)
   Tipocan(tmpb) = Tipocaneep(tmpb)
   Print #1 , "Tipo(" ; Tmpb ; ")=" ; Tipocan(tmpb)
   Unican(tmpb) = Unicaneep(tmpb)
   Print #1 , "Unidad(" ; Tmpb ; ")=" ; Unican(tmpb)

Next

Idest = Idesteep
Print #1 , "IDest=" ; Idest

Tadq = Tadqeep
Print #1 , "Tadq=" ; Tadq

Tac = Taceep
Print #1 , "Tac=" ; Tac

For Tmpb = 1 To Numtxaut
   Autoval(tmpb) = Autovaleep(tmpb)
   Tmpstr52 = Time(autoval(tmpb))
   Print #1 , "AUTO" ; Tmpb ; "=" ; Autoval(tmpb) ; "," ; Tmpstr52
Next

Enaauto = Enaautoeep
Print #1 , "ENAAUTO=" ; Bin(enaauto)

Print #1 , "Modo de Operación=";
If Pwrsta = 1 Then
   Set Mop
   Mopstr = "AC"
   Print #1 , "1"
Else
   Reset Mop
   Print #1 , "0"
   Mopstr = "BAT"
End If
Cntrtramas = Cntrtramaseep
Print #1 , "CNTR Tramas=" ; Cntrtramas

Imei = Imeieep
'Imei = "IMEI MDC"
Print #1 , "IMEI <" ; Imei ; ">"
Numserialwtch = Numserialwtcheep
'Numserialwtch = "WAGGER-001"
Print #1 , "Num serial WTCH=" ; Numserialwtch
Idserial = Idserialeep
'Idserial = "441585188190004058AEA00020000101"
Print #1 , "ID serial WTCH=" ; Idserial

   Ptrnotx = Ptrnotxeep
   Print #1 , "PtrNOTX=" ; Ptrnotx
   Filenotx = "NO_" + Str(ptrnotx) + ".TXT"
   Print #1 , "FileNOTX=" ; Filenotx
   Iptx = Iptxeep
   Print #1 , "IPtx=" ; Iptx
   Ptotx = Ptotxeep
   Print #1 , "PTOtx=" ; Ptotx
   Apn = Apneep
   Print #1 , "APN=" ; Apn



   For Tmpb = 1 To Numtlf
      Tlf(tmpb) = Tlfeep(tmpb)
      Print #1 , "TLF" ; Tmpb ; "=" ; Tlf(tmpb)
   Next

   Trtx = Trtxeep
   Print #1 , "Trtx=" ; Trtx

   Print #1 , "Verificando Directorios"
   Reset Pwrsd
   Wait 2
   Print #1 , "SD ON"
   Tmpb = 0
   Estado_led = 3
   Do
      Incr Tmpb
      Print #1 , "Int. " ; Tmpb
      Tmpstr8 = "DATOS."
      Call Verdir()
      If Sdinitok = 1 Then
         If Tmpw = 0 Then
            Chdir "\"
            Mkdir Tmpstr8
         End If
      End If
   Loop Until Tmpw = 1 Or Tmpb > 10

   Tmpb = 0
   Do
      Incr Tmpb
      Print #1 , "Int. " ; Tmpb
      Tmpstr8 = "LOG."
      Call Verdir()
      If Sdinitok = 1 Then
         If Tmpw = 0 Then
            Chdir "\"
            Mkdir Tmpstr8
         End If
      End If
   Loop Until Tmpw = 1 Or Tmpb > 10

   Tmpb = 0
   Do
      Incr Tmpb
      Print #1 , "Int. " ; Tmpb
      Tmpstr8 = "NOTX."
      Call Verdir()
      If Sdinitok = 1 Then
         If Tmpw = 0 Then
            Chdir "\"
            Mkdir Tmpstr8
         End If
      End If
   Loop Until Tmpw = 1 Or Tmpb > 10

   Tmpb = 0
   Do
      Incr Tmpb
      Print #1 , "Int. " ; Tmpb
      Tmpstr8 = "CONFIG."
      Call Verdir()
      If Sdinitok = 1 Then
         If Tmpw = 0 Then
            Chdir "\"
            Mkdir Tmpstr8
         End If
      End If
   Loop Until Tmpw = 1 Or Tmpb > 10

   If Errorrtc = 0 Then
      Estado_led = 1
   Else
      Estado_led = 2
   End If

   Numsampleb = 30

   Tmpb = 0
   Cntrpwrsta = 0
   Do
      If Pwrsta = 0 Then
         Incr Cntrpwrsta
      End If
      Incr Tmpb
      waitms 10
   Loop Until Tmpb = 10

   If Cntrpwrsta > 6 Then
      Reset Rts
   Else
      Set Rts
   End If

   Print #1 , "RST=" ; Rts
   Cntrpwrsta = 0

   If Cts = 0 Then
      Ctsant = 1
   Else
      Ctsant = 0
   End If
   Print #1 , "CTS=" ; Cts
   Filelog = Filelogeep
   Print #1 , "FILElog <" ; Filelog ; ">"

   If Filelog = "" Then
      Filelog = "LOG.TXT"
   End If
   Horamin = Horamineep
   Print #1 , "Hmin=" ; Horamin
   Print #1 , Time(horamin)
   Print #1 , Date(horamin)

   Estadogps = Estadogpseep
   Print #1 , "Estado GPS=" ; Estadogps

   Latitud = Latitudeep
   Print #1 , "LAT=" ; Latitud

   Longitud = Longitudeep
   Print #1 , "LON=" ; Longitud

   'Set Pwrmdc
'   Print #1 , "RST LOW"
'   Set Rstmdc
'   Wait 2
   'Reset Rstmdc
   'Print #1 , "RST HIGH"

End Sub


'*******************************************************************************
' Procesamiento de comandos
'*******************************************************************************
Sub Procser()
   'Print #1 , "$" ; Serproc
   'Tmpstr52 = Mid(serproc , 1 , 6)
   Numpar = Split(serproc , Cmdsplit(1) , ",")
   'If Numpar > 0 Then
   '   For Tmpb = 1 To Numpar
   '      Print #1 , Tmpb ; ":" ; Cmdsplit(tmpb)
   '   Next
   'End If
   Atsnd = Serproc + "," + Time$ + "," + Date$
   Call Writesd( "CMD_LOG.TXT" , Atsnd , "LOG")

   If Len(cmdsplit(1)) = 6 Then
      Cmdtmp = Cmdsplit(1)
      Cmdtmp = Ucase(cmdtmp)
      Cmderr = 255
      Select Case Cmdtmp
         Case "LEEVFW"
            Cmderr = 0
            Atsnd = "Version FW: Fecha <"
            Tmpstr52 = Version(1)
            Atsnd = Atsnd + Tmpstr52 + ">; Archivo <"
            Tmpstr52 = Version(3)
            Atsnd = Atsnd + Tmpstr52 + ">"

        Case "SETCLK"
            If Numpar = 2 Then
               If Len(cmdsplit(2)) = 12 Then
                  Cmderr = 0
                  Horatmp = Mid(cmdsplit(2) , 7 , 2) + ":" + Mid(cmdsplit(2) , 9 , 2) + ":" + Mid(cmdsplit(2) , 11 , 2)
                  Print #1 , Horatmp
                  'Time$ = Tmpstr52
                  'Print #1 , "T>" ; Time$
                  Fechatmp = Mid(cmdsplit(2) , 1 , 2) + "/" + Mid(cmdsplit(2) , 3 , 2) + "/" + Mid(cmdsplit(2) , 5 , 2)
                  Print #1 , Fechatmp
                  'Date$ = Tmpstr52
                  'Print #1 , "D>" ; Date$
                  Tmpl = Syssec(horatmp , Fechatmp)
                  Horatmp = Time(tmpl)
                  Print #1 , "T=" ; Horatmp
                  Time$ = Horatmp
                  Fechatmp = Date(tmpl)
                  Date$ = Fechatmp
                  Print #1 , "D=" ; Fechatmp
                  Atsnd = "WATCHING INFORMA. Se configuro reloj en " + Fechatmp + " a " + Horatmp
                  Dow = Dayofweek()
                  Call Setdateds3231()
                  Call Settimeds3231()
                  Call Getdatetimeds3231()
                  Horamin = Syssec()
                  'Horamin = Tmpl
                  Horamineep = Horamin
                  'Set Actclkok
               Else
                  Cmderr = 6
               End If
            Else
               Cmderr = 4
            End If

         Case "SETINI"
            If Numpar = 2 Then
               Cntrini = Val(cmdsplit(2))
               Cntrinieep = Cntrini
               Cmderr = 0
               Atsnd = "Se configuro Contador Inicios: " + Str(cntrini)
            Else
               Cmderr = 4
            End If

         Case "LEEINI"
            If Numpar = 1 Then
               Cmderr = 0
               Atsnd = "Contador inicios <" + Str(cntrini) + ">"
            Else
               Cmderr = 4
            End If

         Case "LEERTC"
            Cmderr = 0
            Atsnd = "Lee RTC"
            Tmpstr52 = Time$
            Atsnd = "Lee RTC a" + Tmpstr52
            Call Getdatetimeds3231()
            tmpstr52=time$
            Atsnd = Atsnd + ";" + Tmpstr52

         Case "SISCLK"
            Cmderr = 0
            Tmpl = Syssec()
            Tmpstr52 = Time(tmpl)
            Atsnd = "Hora actual=" + Tmpstr52 + "; Fecha actual="
            Tmpstr52 = Date$
            Atsnd = Atsnd + Tmpstr52

         Case "LEECLK"
            Cmderr = 0
            Tmpstr52 = Time(horamin)
            Atsnd = "Ultima ACT CLK a =" + Tmpstr52 + "; del "
            Tmpstr52 = Date(horamin)
            Atsnd = Atsnd + Tmpstr52

         Case "SETLED"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb < 17 Then
                  Cmderr = 0
                  Atsnd = "Se configura setled a " + Str(tmpb)
                  Estado_led = Tmpb
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "PWRMDC"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb < 2 Then
                  If Tmpb = 0 Then
                   '  Reset Pwrmdc
                  Else
                    ' Set Pwrmdc
                  End If
                  Cmderr = 0
                 ' Atsnd = "Se configuro PWRMDC=" + Str(pwrmdc)
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "PWRTSD"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb < 2 Then
                  If Tmpb = 0 Then
                     Reset Pwrsd
                  Else
                     Set Pwrsd
                  End If
                  Cmderr = 0
                  Atsnd = "Se configuro PWR sd=" + Str(pwrsd)
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "LSTFIL"
            Cmderr = 0
            Select Case Numpar
               Case 1
                  Call Diskinsertion()
                  If Sdinitok = 1 Then
                     Cmderr = 0
                     Tmpw = 0
                     Chdir "\"                        ' Para ir al directorio raiz
                     Dirstr = Dir( "*.*")
                     Tmpstr8 = "*.*"
                     Print #1 , "$INIDIR"
                     If Len(dirstr) > 0 Then
                        While Len(dirstr) > 0               ' if there was a file found
                           Tmpstr8 = Filedatetime(dirstr)
                           Print #1 , Dirstr ; "," ; Filelen(dirstr) ; "," ; Tmpstr8
                           Reset Watchdog
                           Dirstr = Dir()
                           Incr Tmpw
                        Wend
                     End If
                     Print #1 , "$FINDIR"
                     Atsnd = "Existen " + Str(tmpw) + " archivos " + Tmpstr8 + " en DIR \"
                  Else
                     Cmderr = 9
                  End If

               Case 2
                  Call Diskinsertion()
                  If Sdinitok = 1 Then
                     Cmderr = 0
                     Tmpstr8 = Cmdsplit(2)
                     Atsnd = "Archivos SD tipo " + Tmpstr8 + " en DIR \"
                     Chdir "\"                        ' Para ir al directorio raiz
                     Tmpw = 0
                     Dirstr = Dir(tmpstr8)
                     Print #1 , "$INIFIL"
                     If Len(dirstr) > 0 Then
                        While Len(dirstr) > 0               ' if there was a file found
                           Tmpstr8 = Filedatetime(dirstr)
                           Print #1 , Dirstr ; "," ; Filelen(dirstr) ; "," ; Tmpstr8
                           Reset Watchdog
                           Dirstr = Dir()
                           Incr Tmpw
                        Wend
                     End If
                     Print #1 , "$FINFIL"
                     Atsnd = "Existen " + Str(tmpw) + " archivos " + Tmpstr8
                  Else
                     Cmderr = 9
                  End If

               Case 3
                  Call Diskinsertion()
                  If Sdinitok = 1 Then
                     Cmderr = 0
                     Tmpstr8 = Cmdsplit(2)
                     Tmpstr52 = Cmdsplit(3)
                     'Atsnd = "Archivos SD tipo " + Tmpstr8 + " en DIR " + Tmpstr52
                     Print #1 , "$INIFIL"
                     Chdir "\"                              ' Para ir al directorio raiz
                     Chdir Tmpstr52
                     Tmpw = 0
                     Dirstr = Dir(tmpstr8)
                     If Len(dirstr) > 0 Then
                        While Len(dirstr) > 0               ' if there was a file found
                           Tmpstr8 = Filedatetime(dirstr)
                           Print #1 , Dirstr ; "," ; Filelen(dirstr) ; "," ; Tmpstr8
                           Reset Watchdog
                           Dirstr = Dir()
                           Incr Tmpw
                        Wend
                     End If
                     Print #1 , "$FINFIL"
                     Atsnd = "Existen " + Str(tmpw) + " archivos " + Tmpstr8
                  Else
                     Cmderr = 9
                  End If

               Case Else
                  Cmderr = 5

            End Select

         Case "LEEFIL"
            If Numpar = 2 Or Numpar = 3 Then
               Tmpstr52 = Cmdsplit(2)
               Tmpstr8 = Cmdsplit(3)
               Chdir "\"
               If Numpar = 3 Then
                  Chdir Tmpstr8
               End If

               Dirstr = Dir(tmpstr52)
               If Len(dirstr) > 0 Then
                  Cmderr = 0
                  Set Inilog
                  Tmpl2 = Filelen(dirstr)
                  'Tmps = Tmpl2 / 5760
                  Tmps = Tmpl2 / 960
                  'Print #1 , "Por favor espere " ; Fusing(tmps , "#.#") ; " seg aproximadamente"
                  Atsnd = "Lee Log " + Cmdsplit(2) + " en DIR "
                  If Numpar = 3 Then
                     Atsnd = Atsnd + Tmpstr8
                     Tmpdir = Tmpstr8
                  Else
                     Atsnd = Atsnd + "\"
                     Tmpdir = "\"
                  End If
               Else
                  Cmderr = 23
               End If
            Else
               Cmderr = 5
            End If


         Case "DELFIL"
            'If Numpar = 3 Then
            If Numpar = 2 Or Numpar = 3 Then
               Print #1 , "Borrar Archivo " ; Cmdsplit(2) ; " a " ; Date$ ; "," ; Time$
               Tmpstr52 = Cmdsplit(2)
               Tmpstr8 = Cmdsplit(3)
               Chdir "\"
               If Numpar = 3 Then
                  Chdir Tmpstr8
               End If

               Dirstr = Dir(tmpstr52)
               If Len(dirstr) > 0 Then
                  Print #1 , "Dirstr>" ; Dirstr
                  Disable Urxc
                  Call Diskinsertion()
                  If Sdinitok = 1 Then
                     Cmderr = 0
                     Chdir "\"
                     Chdir Tmpstr8
                     Atsnd = " Borra Archivo " + Cmdsplit(2)
                     Dirstr = Cmdsplit(2)
                     Reset Watchdog
                     Kill Dirstr
                     Flush
                  Else
                     Cmderr = 27
                  End If
                  Enable Urxc
               Else
                   Cmderr = 23
               End If
            Else
               Cmderr = 5
            End If

         Case "RNMFIL"                                'Renombra archivo
            If Numpar = 4 Then
               'Cmderr = 0
               Tmpstr8 = Cmdsplit(4)
               Tmpstr52 = Cmdsplit(3)
               Chdir "\"
               Chdir Tmpstr8
               Tmpstr8 = Cmdsplit(2)
               Dirstr = Dir(tmpstr8)
               If Len(dirstr) > 0 Then
                  Print #1 , "Dirstr>" ; Dirstr
                  If Len(tmpstr52) > 1 And Len(tmpstr52) < 13 Then
                     Cmderr = 0
                     Name Dirstr As Tmpstr52
                     Atsnd = " Se renombro " + Tmpstr8 + " a " + Tmpstr52
                  Else
                     Cmderr = 24
                  End If
               Else
                  Cmderr = 23
               End If
            Else
               Cmderr = 5
            End If

         Case "MAKDIR"                                'Crea directorio
            If Numpar = 2 Then
               If Len(cmdsplit(2)) < 9 Then
                  Call Diskinsertion()
                  If Sdinitok = 1 Then
                     Cmderr = 0
                     Tmpstr52 = Cmdsplit(2)
                     Print #1 , Tmpstr52
                     Mkdir Tmpstr52
                     Atsnd = "Se creo directorio <" + Tmpstr52 + ">"
                  Else
                     Cmderr = 27
                  End If
               Else
                  Cmderr = 24
               End If

            Else
               Cmderr = 5
            End If

         Case "RMVDIR"
            If Numpar = 2 Then
               If Len(cmdsplit(2)) < 9 Then
                  Call Diskinsertion()
                  If Sdinitok = 1 Then
                     Cmderr = 0
                     Tmpstr52 = Cmdsplit(2)
                     Print #1 , Tmpstr52
                     Rmdir Tmpstr52
                     Atsnd = "Se borro directorio <" + Tmpstr52 + ">"
                  Else
                     Cmderr = 27
                  End If
               Else
                  Cmderr = 24
               End If
            Else
               Cmderr = 5
            End If

         Case "CHGDIR"
            If Numpar = 2 Then
               If Len(cmdsplit(2)) < 9 Then
                  Call Diskinsertion()
                  If Sdinitok = 1 Then
                     Cmderr = 0
                     Tmpstr52 = Cmdsplit(2)
                     Atsnd = "Directorio Actual <" + Tmpstr52 + ">"
                     Chdir "\"
                     Chdir Tmpstr52
                  Else
                     Cmderr = 27
                  End If
               Else
                  Cmderr = 24
               End If
            Else
               Cmderr = 5
            End If

         Case "ENASDB"
            If Numpar = 2 Then
               Cmderr = 0
               Debugsd = Val(cmdsplit(2))
               Atsnd = "Se configuro DEBUGSD=" + Str(debugsd)
            End If

         Case "LEESDB"
            Cmderr = 0
            Atsnd = "DEBUGSD=" + Str(debugsd)

         Case "ENABUG"
            If Numpar = 2 Then
               Cmderr = 0
               Enabug = Val(cmdsplit(2))
               Enabugeep = Enabug
               Atsnd = "Se configuro ENABUG=" + Str(enabug)
            Else
               Cmderr = 4
            End If

         Case "LEEBUG"
            Cmderr = 0
            Atsnd = "ENABUG=" + Str(enabug)

         Case "SETADC"
            If Numpar = 3 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb > 0 And Tmpb < Numadc_masuno Then
                  Cmderr = 0
                  Kadc(tmpb) = Val(cmdsplit(3))
                  'Tmps = Val(cmdsplit(3))
                  Kadceep(tmpb) = Kadc(tmpb)
                  Atsnd = "Se configuro Kadc" + Str(tmpb) + "=" + Str(kadc(tmpb))
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "LEEADC"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb > 0 And Tmpb < Numadc_masuno Then
                  Cmderr = 0
                  Atsnd = "Kadc" + Str(tmpb) + "=" + Str(kadc(tmpb))
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "LEEVDC"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb > 0 And Tmpb < Numadc_masuno Then
                  Cmderr = 0
                  Atsnd = "Vdc" + Str(tmpb) + "=" + Str(adc_data(tmpb))
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "LEEVMB"
            Cmderr = 0
            Atsnd = "V mainboard=" + Fusing(vmain , "#.##")

         Case "LEEVMD"
            Cmderr = 0
            Atsnd = "V mdc=" + Fusing(vmdc , "#.##")

         Case "SETCAN"
            If Numpar = 4 Then
               Tmpb = Val(cmdsplit(2))                      'Numero de canal
               If Tmpb < Numcan_masuno Then
                  Tmpb2 = Val(cmdsplit(3))                  'Parametro
                  Select Case Tmpb2
                     Case 1:                                'ID
                        Cmderr = 0
                        Set Initxinicio
                        Idcan(tmpb) = Cmdsplit(4)
                        Idcaneep(tmpb) = Idcan(tmpb)
                        Atsnd = "Se configuro ID(" + Str(tmpb) + ")=" + Idcan(tmpb)
                     Case 2:                                'Iniescala
                        Cmderr = 0
                        Set Initxinicio
                        Iniescala(tmpb) = Val(cmdsplit(4))
                        Iniescalaeep(tmpb) = Iniescala(tmpb)
                        Atsnd = "Se configuro IniEscala(" + Str(tmpb) + ")=" + Str(iniescala(tmpb))
                        Mval(tmpb) = Fonescala(tmpb) - Iniescala(tmpb)
                        Mval(tmpb) = Mval(tmpb) / 52428
                        Bval(tmpb) = 5 * Iniescala(tmpb)
                        Bval(tmpb) = Bval(tmpb) - Fonescala(tmpb)
                        Bval(tmpb) = Bval(tmpb) / 4

                     Case 3:                                'Fondo escala
                        Cmderr = 0
                        Set Initxinicio
                        Fonescala(tmpb) = Val(cmdsplit(4))
                        Fonescalaeep(tmpb) = Fonescala(tmpb)
                        Atsnd = "Se configuro FonEscala(" + Str(tmpb) + ")=" + Str(fonescala(tmpb))
                        Mval(tmpb) = Fonescala(tmpb) - Iniescala(tmpb)
                        Mval(tmpb) = Mval(tmpb) / 52428
                        Bval(tmpb) = 5 * Iniescala(tmpb)
                        Bval(tmpb) = Bval(tmpb) - Fonescala(tmpb)
                        Bval(tmpb) = Bval(tmpb) / 4

                     Case 4:                                'Habilitacion
                        Tmpb3 = Val(cmdsplit(4))
                        If Tmpb3 < 2 Then
                           Cmderr = 0
                           Set Initxinicio
                           Tmpb4 = Tmpb - 1
                           If Tmpb3 = 0 Then
                              Reset Enacan.tmpb4
                           Else
                              Set Enacan.tmpb4
                           End If
                           Enacaneep = Enacan
                           Atsnd = "Habilitacion canal " + Str(tmpb) + "=" + Str(tmpb3)
                        Else
                           Cmderr = 7
                        End If

                     Case 5:
                        Tmpb3 = Val(cmdsplit(4))
                        If Tmpb3 < 10 Then
                           Cmderr = 0
                           Set Initxinicio
                           Ndcan(tmpb) = Tmpb3
                           Ndcaneep(tmpb) = Tmpb3
                           Atsnd = "Num. decimales " + Str(tmpb) + "=" + Str(tmpb3)
                        Else
                           Cmderr = 7
                        End If

                     Case 6:
                        If Len(cmdsplit(4)) < 7 Then
                           Cmderr = 0
                           Set Initxinicio
                           Tipocan(tmpb) = Cmdsplit(4)
                           Tipocaneep(tmpb) = Tipocan(tmpb)
                           Atsnd = "Tipo " + Str(tmpb) + "=" + Tipocan(tmpb)
                        Else
                           Cmderr = 7
                        End If


                     Case 7:
                        If Len(cmdsplit(4)) < 7 Then
                           Cmderr = 0
                           Set Initxinicio
                           Unican(tmpb) = Cmdsplit(4)
                           Unicaneep(tmpb) = Unican(tmpb)
                           Atsnd = "Unidad " + Str(tmpb) + "=" + Unican(tmpb)
                        Else
                           Cmderr = 7
                        End If

                     Case Else
                        Cmderr = 6
                  End Select
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "SETKCA"
            If Numpar = 3 Then
               Tmpb = Val(cmdsplit(2))                      'Numero de canal
               If Tmpb < Numcan_masuno Then
                  Tmps = Val(cmdsplit(3))
                  If Tmps > 0 Then
                     Cmderr = 0
                     Kcal(tmpb) = Tmps
                     Kcaleep(tmpb) = Tmps
                     Atsnd = "Se configuro Kcal(" + Str(tmpb) + ")=" + Str(kcal(tmpb))
                  Else
                     Cmderr = 6
                  End If
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If


         Case "LEEKCA"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))                      'Numero de canal
               If Tmpb < Numcan_masuno Then
                  Cmderr = 0
                  Atsnd = "Kcal(" + Str(tmpb) + ")=" + Str(kcal(tmpb))
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "LEECAN"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))                      'Numero de canal
               If Tmpb < Numcan_masuno Then
                  Cmderr = 0
                  Atsnd = "Canal " + Str(tmpb) + "; ID=" + Idcan(tmpb) + "; IniEscala=" + Str(iniescala(tmpb))
                  Tmpb2 = Tmpb - 1
                  Atsnd = Atsnd + "; FonEscala=" + Str(fonescala(tmpb)) + "; HAB=" + Str(enacan.tmpb2)
                  Atsnd = Atsnd + "; ND=" + Str(ndcan(tmpb)) + "; T=" + Tipocan(tmpb)
                  Atsnd = Atsnd + "; UNI=" + Unican(tmpb)
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "INIVAR"
            Cmderr = 0
            Set Inivariables
            Atsnd = "Reinicio de variables"

         Case "RSTVAR"
            Cmderr = 0
            Atsnd = "Se configuran variables a valores por defecto"
            Set Inivariables
            Cfgok = 1
            Cfgokeep = Cfgok
            Call Defaultvalues()

         Case "SETCFG"
            If Numpar = 2 Then
               Cfgok = Val(cmdsplit(2))
               Cfgokeep = Cfgok
               Cmderr = 0
               Atsnd = "Se configura CFGOK=" + Str(cfgok)
            Else
               Cmderr = 4
            End If

         Case "LEESTA"
            If Numpar = 1 Then
               Cmderr = 0
               If Pwrsta = 1 Then
                  Atsnd = "PWRSTA=1"
               Else
                  Atsnd = "PWRSTA=0"
               End If

            Else
               Cmderr = 4
            End If

         Case "SETTXP"
            If Numpar = 3 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb > 0 And Tmpb < Numtxaut_masuno Then
                  If Len(cmdsplit(3)) = 8 Then
                     Numpar = Split(cmdsplit(3) , Horasplit(1) , ":")
                     If Numpar = 3 Then
                        Cmderr = 0
                        Set Initxinicio
                        Autoval(tmpb) = Secofday(cmdsplit(3))
                        Autovaleep(tmpb) = Autoval(tmpb)
                        Tmpstr52 = Time(autoval(tmpb))
                        Atsnd = "Se configuro Tx Per" + Str(tmpb) + "=" + Tmpstr52
                     Else
                        Cmderr = 6
                     End If
                  Else
                     Cmderr = 6
                  End If
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

            Case "LEETXP"
               If Numpar = 2 Then
                  Tmpb = Val(cmdsplit(2))
                  If Tmpb > 0 And Tmpb < Numtxaut_masuno Then
                     Cmderr = 0
                     Tmpstr52 = Time(autoval(tmpb))
                     Atsnd = "Tx Per" + Str(tmpb) + "=" + Tmpstr52
                  Else
                     Cmderr = 5
                  End If
               Else
                  Cmderr = 4
               End If

            Case "SETAUT"
               If Numpar = 3 Then
                  Tmpb = Val(cmdsplit(2))
                  If Tmpb > 0 And Tmpb < Numtxaut_masuno Then
                     Tmpb2 = Val(cmdsplit(3))
                     If Tmpb2 < 2 Then
                        Cmderr = 0
                        Set Initxinicio
                        Tmpb3 = Tmpb - 1
                        If Tmpb2 = 1 Then
                           Set Enaauto.tmpb3
                        Else
                           Reset Enaauto.tmpb3
                        End If
                        Enaautoeep = Enaauto
                        Atsnd = "Habilitacion Tx Per" + Str(tmpb) + "=" + Str(tmpb2)
                     Else
                        Cmderr = 6
                     End If
                  Else
                     Cmderr = 5
                  End If
               Else
                  Cmderr = 4
               End If

            Case "LEEAUT"
               If Numpar = 2 Then
                  Tmpb = Val(cmdsplit(2))
                  If Tmpb > 0 And Tmpb < Numtxaut_masuno Then
                     Cmderr = 0
                     Tmpb3 = Tmpb - 1
                     Tmpbit = Enaauto.tmpb3
                     Atsnd = "Tx Per" + Str(tmpb) + "=" + Str(tmpbit)
                  Else
                     Cmderr = 5
                  End If
               Else
                  Cmderr = 4
               End If

         Case "SETTAD"                                      'Tiempo de adquisisción de datos (muestreo)
            If Numpar = 2 Then
               Tmpw = Val(cmdsplit(2))
               If Tmpw > 0 Then
                  Cmderr = 0
                  Set Initxinicio
                  Tadq = Tmpw
                  Atsnd = "Se configuro Tadq=" + Str(tadq)
                  Tadqeep = Tadq
                  Numsamples = Tac / Tadq
                  Numsampleb = Numsamples
                  Atsnd = Atsnd + ", Numsample=" + Str(numsampleb)
                  Cntrsamples = 0
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "LEETAD"
            Cmderr = 0
            Atsnd = "Tadq=" + Str(tadq)
            Atsnd = Atsnd + ", Numsample=" + Str(numsampleb)

         Case "SETTAC"                                      'Tiempo de acumulación (registro)
            If Numpar = 2 Then
               Tmpw = Val(cmdsplit(2))
               If Tmpw > 59 Then
                  Cmderr = 0
                  Set Initxinicio
                  Tac = Tmpw
                  Atsnd = "Se configuro Tacumulacion=" + Str(tac)
                  Taceep = Tac
                  Numsamples = Tac / Tadq
                  Numsampleb = Numsamples
                  Atsnd = Atsnd + ", Numsample=" + Str(numsampleb)
                  Cntrsamples = 0
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "LEETAC"
            Cmderr = 0
            Atsnd = "Tac=" + Str(tac)
            Atsnd = Atsnd + ", Numsample=" + Str(numsampleb)

         Case "SETIDE"
            If Numpar = 3 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb > 0 And Tmpb < 10 Then
                  Select Case Tmpb
                     Case 1:                                'ID estacion
                        If Len(cmdsplit(3)) < 33 Then
                           Cmderr = 0
                           Set Initxinicio
                           Idest = Cmdsplit(3)
                           Idesteep = Idest
                           Atsnd = "Se configuro ID estacion a <" + Idest + ">"
                        Else
                           Cmderr = 6
                        End If
                     Case 3:                                'APN
                        Cmderr = 0
                        Set Initxinicio
                        Apn = Cmdsplit(3)
                        Apneep = Apn
                        Atsnd = "Se configuro APN a <" + Apn + ">"
                     Case 9:                                ' SERIAL WATCHING
                        Cmderr = 0
                        Set Initxinicio
                        Numserialwtch = Cmdsplit(3)
                        Numserialwtcheep = Numserialwtch
                        Atsnd = "Se config. num. serial WATCHING a " + Numserialwtch
                     Case Else:
                        Cmderr = 0
                        Atsnd = "No aplica en esta version"

                  End Select

               Else
                  Cmderr = 5
               End If

            Else
               Cmderr = 4
            End If

         Case "LEEIDE"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               Select Case Tmpb:
                  Case 1:
                     Cmderr = 0
                     Atsnd = "ID estacion <" + Idest + ">"
                  Case 3:
                     Cmderr = 0
                     Atsnd = "APN <" + Apn + ">"
                  Case 9:
                     Cmderr = 0
                     Atsnd = "Num. serial WATCHING <" + Numserialwtch + ">"
                  Case Else:
                     Cmderr = 0
                     Atsnd = "No aplica esta version"
               End Select
            Else
               Cmderr = 5
            End If


         Case "INIPRG"
            Cmderr = 0
            Atsnd = "Ini modo PROG"
            Set Iniprg

         Case "FINPRG"
            Cmderr = 0
            Atsnd = "Fin modo PROG"
            Reset Iniprg

         Case "TSTINI"
            Cmderr = 0
            Atsnd = "Test TX trama inicio"
            Set Initxinicio

         Case "SETCTP"                                      'Contador de tramas pendientes
            If Numpar = 2 Then
               Cmderr = 0
               Cntrtramas = Val(cmdsplit(2))
               Cntrtramaseep = Cntrtramas
               Atsnd = "Se configuro contador de tramas pendientes a" + Str(cntrtramas)
            Else
               Cmderr = 4
            End If

         Case "LEECTP"
            Cmderr = 0
            Atsnd = "Contador de tramas pendientes =" + Str(cntrtramas)

         Case "SETPTR"
            If Numpar = 2 Then
               Cmderr = 0
               Ptrnotx = Val(cmdsplit(2))
               Ptrnotxeep = Ptrnotx
               Atsnd = "Se config. ptr archivos NOTx a " + Str(ptrnotx)
               Filenotx = "NO_" + Str(ptrnotx) + ".TXT"
            Else
               Cmderr = 4
            End If

         Case "LEEPTR"
            Cmderr = 0
            Atsnd = "PTR archivos NOTx =" + Str(ptrnotx)

         Case "SETIPX"
            If Numpar = 2 Then
               Cmderr = 0
               Iptx = Cmdsplit(2)
               Iptxeep = Iptx
               Atsnd = "Se configuro Ip <" + Iptx + ">"
            Else
               Cmderr = 4
            End If

         Case "SETPTX"
            If Numpar = 2 Then
               Cmderr = 0
               Ptotx = Cmdsplit(2)
               Ptotxeep = Ptotx
               Atsnd = "Se configuro Puerto <" + Ptotx + ">"
            Else
               Cmderr = 4
            End If

         Case "LEEIPX"
            If Numpar = 1 Then
               Cmderr = 0
               Atsnd = "Ip <" + Iptx + ">"
            Else
               Cmderr = 4
            End If

         Case "LEEPTX"
            If Numpar = 1 Then
               Cmderr = 0
               Atsnd = "Puerto <" + Ptotx + ">"
            Else
               Cmderr = 4
            End If


         Case "SETAPN"
            If Numpar = 2 Then
               Cmderr = 0
               Set Initxinicio
               Apn = Cmdsplit(2)
               Apneep = Apn
               Atsnd = "Se configuro APN a <" + Apn + ">"
            Else
               Cmderr = 4
            End If

         Case "LEEAPN"
            Cmderr = 0
            Atsnd = "APN <" + Apn + ">"

         Case "SETTLF"
            If Numpar = 3 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb < Numtlf_masuno Then
                  Cmderr = 0
                  Set Initxinicio
                  Tlf(tmpb) = Cmdsplit(3)
                  Tlfeep(tmpb) = Tlf(tmpb)
                  Atsnd = "Se config. TLF" + Str(tmpb) + "=" + Tlf(tmpb)
               Else
                  Cmderr = 4
               End If
            Else
               Cmderr = 4
            End If

         Case "LEETLF"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb < Numtlf_masuno Then
                  Cmderr = 0
                  Atsnd = "TLF" + Str(tmpb) + "=" + Tlf(tmpb)
               Else
                  Cmderr = 4
               End If
            Else
               Cmderr = 4
            End If

         Case "SETRTX"
            If Numpar = 2 Then
               Cmderr = 0
               Set Initxinicio
               Trtx = Val(cmdsplit(2))
               Trtxeep = Trtx
               Atsnd = "Se config. Tiempo entre rtx a " + Str(trtx)
            Else
               Cmderr = 4
            End If

         Case "LEERTX"
            Cmderr = 0
            Atsnd = "Tiempo entre rtx =" + Str(trtx)

         Case "INIRTX"
            Cmderr = 0
            Set Inirtx
            Atsnd = "Inicio RTX"

         Case "TSTADC"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))                      'Numero de canal
               If Tmpb < Numcan_masuno Then
                  Cmderr = 0
                  If Tmpb > 0 Then
                     Set Initstadc
                     Canaltst = Tmpb
                     Atsnd = "Ini prueba de canal " + Str(canaltst)
                  Else
                     Reset Initstadc
                     Atsnd = "FIN tst ADC"
                  End If


               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "LEECTS"
            Cmderr = 0
            Atsnd = "CTS=" + Str(cts)

         Case "SETRTS"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb < 2 Then
                  Cmderr = 0
                  If Tmpb = 1 Then
                     Set Rts
                  Else
                     Reset Rts
                  End If
                  Atsnd = "SET RTS=" + Str(rts)
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "LEERTS"
            Cmderr = 0
            Atsnd = "RTS=" + Str(rts)

         Case "LEESER"
            Cmderr = 0
            Atsnd = "SER=" + Idserial

         Case "LEEGSN"
            Cmderr = 0
            Atsnd = "IMEI=" + Imei

         Case "LEEGSM"
            Cmderr = 0
            Atsnd = "GSM=" + Gsmval

         Case "SETWSN"
            If Numpar = 2 Then
               Cmderr = 0
               Set Initxinicio
               Numserialwtch = Cmdsplit(2)
               Numserialwtcheep = Numserialwtch
               Atsnd = "Se config. num. serial WATCHING a " + Numserialwtch
            Else
               Cmderr = 4
            End If

         Case "LEEWSN"
            Cmderr = 0
            Atsnd = "Num serial WATCHING =" + Numserialwtch

         Case "DORMIR"
            Cmderr = 0
            Set Pwrsave
            Atsnd = "TST SLEEP"

         Case "SETMDC"
            Select Case Numpar
               Case 2:
                  Tmpstr52 = "%" + Cmdsplit(2)
                  Cmderr = 0
                  Atsnd = "CMDMDC>" + Tmpstr52
               Case 3:
                  Tmpstr52 = "%" + Cmdsplit(2) + "," + Cmdsplit(3)
                  Cmderr = 0
                  Atsnd = "CMDMDC>" + Tmpstr52
               Case Else:
                  Cmderr = 4
            End Select
            If Cmderr = 0 Then
               Print #1 , Tmpstr52
               Print #2 , Tmpstr52
            End If

         Case "LEEMBV"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb > 0 And Tmpb < Numcan_masuno Then
                  Cmderr = 0
                  Atsnd = "Mval(" + Str(tmpb) + ")=" + Str(mval(tmpb)) + ";Bval=("
                  Atsnd = Atsnd + Str(tmpb) + ")=" + Str(bval(tmpb))
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "SETLAT"
            If Numpar = 2 Then
               Cmderr = 0
               Set Initxinicio
               Latitud = Cmdsplit(2)
               Latitudeep = Latitud
               Atsnd = "Se configuro LAT GPS a " + Latitud
            Else
               Cmderr = 4
            End If

         Case "SETLON"
            If Numpar = 2 Then
               Cmderr = 0
               Set Initxinicio
               Longitud = Cmdsplit(2)
               Longitudeep = Longitud
               Atsnd = "Se configuro LONG GPS a " + Longitud
            Else
               Cmderr = 4
            End If

         Case "SETGPS"
            If Numpar = 2 Then
               Cmderr = 0
               Set Initxinicio
               Estadogps = Cmdsplit(2)
               Estadogpseep = Estadogps
               Atsnd = "Se configuro Estado GPS a " + Estadogps
            Else
               Cmderr = 4
            End If

         Case "LEEGPS"
            Cmderr = 0
            Set Initxinicio
            Atsnd = "LAT=" + Latitud + ";LONG=" + Longitud + ";STA=" + Estadogps


         Case "SETRST"
            If Numpar = 2 Then
               Cmderr = 0
               Tmpb = Val(cmdsplit(2))
               If Tmpb = 0 Then
                  Reset Rstmdc
               Else
                  Set Rstmdc
               End If
               Atsnd = "RSTMDC=" + Str(rstmdc)
            Else
               Cmderr = 4
            End If

         Case "RSTMDC"
            Cmderr = 0
            Atsnd = "RST MDC"
            Print #1 , "RST LOW"
            Set Rstmdc
            Wait 1
            Reset Rstmdc
            Print #1 , "RST HIGH"

         Case "SETMDB"
            If Numpar = 4 Then
               Cmderr = 0
               Regmdb(1) = Cmdsplit(2)
               Regmdb(2) = Cmdsplit(3)
               Regmdb(3) = Cmdsplit(4)
               Atsnd = "Se configuro registros MDB=" + Regmdb(1) + ";" + Regmdb(2) + ";" + Regmdb(3)

            Else
               Cmderr = 4
            End If

         Case "LEEMDB"
            Cmderr = 0
            Atsnd = "Reg. MDB=" + Regmdb(1) + ";" + Regmdb(2) + ";" + Regmdb(3)

         Case "SETRMD"
            If Numpar = 3 Then
               Cmderr = 0
               Addrmdb = Val(cmdsplit(2))
               Valmdb = Val(cmdsplit(3))
               'Set Iniwrmdb
               Atsnd = "Se configura reg DEC=" + Str(addrmdb) + " a " + Str(valmdb)
               Print#1 , "!SETRMD," ; Addrmdb ; "," ; Valmdb
            Else
               Cmderr = 4
            End If

         Case "LEERMD"
            If Numpar = 2 Then
               Cmderr = 0
               Addrmdb = Val(cmdsplit(2))
               'Set Inirdmdb
               Atsnd = "Ini lectura reg DEC=" + Str(addrmdb) + "; HEX=" + Hex(addrmdb)
               Print#1 , "!LEERMD," ; Addrmdb
            Else
               Cmderr = 4
            End If

         Case "SETRMH"
            If Numpar = 3 Then
               Cmderr = 0
               Addrmdb = Hexval(cmdsplit(2))
               Valmdb = Hexval(cmdsplit(3))
               'Set Iniwrmdb
               Atsnd = "Se configura reg HEX" + Hex(addrmdb) + " a HEX=" + Hex(valmdb) + "; DEC=" + Str(valmdb)
               Print#1 , "!SETRMH," ; Hex(addrmdb) ; "," ; Hex(valmdb)
            Else
               Cmderr = 4
            End If

         Case "LEERMH"
            If Numpar = 2 Then
               Cmderr = 0
               Addrmdb = Hexval(cmdsplit(2))
               'Set Inirdmdb
               Atsnd = "Ini lectura reg HEX=" + Hex(addrmdb) + "; DEC=" + Str(addrmdb)
               Print#1 , "!LEERMH," ; Hex(addrmdb)
            Else
               Cmderr = 4
            End If


         Case "LEERIH"
            If Numpar = 2 Then
               Cmderr = 0
               Addrmdb = Hexval(cmdsplit(2))
               'Set Inirdiprmdb
               Atsnd = "Ini lectura reg InputReg HEX=" + Hex(addrmdb) + "; DEC=" + Str(addrmdb)
               Print#1 , "!LEERIH," ; Hex(addrmdb)
            Else
               Cmderr = 4
            End If

         Case "INITXD"
            Cmderr = 0
            Set Initac
            Atsnd = "Envio trama WAGGER"

'Initac


         Case Else
            Cmderr = 1

      End Select

   Else
        Cmderr = 2
   End If

   If Cmderr > 0 Then
      Atsnd = Lookupstr(cmderr , Tbl_err)
   End If

   Print #1 , Atsnd
   If Cmderr = 0 Then
      Print #1 , "$OK;" ; Serproc ; ";" ; Atsnd
   Else
      Print #1 , "$ERR;" ; Serproc ; ";" ; Atsnd
   End If

End Sub


'*******************************************************************************
'Subrutinas SD
'*******************************************************************************
Sub Diskinsertion()
   Local Errorcode As Byte
   If Debugsd = 1 Then
      Print #1 , "Ini SD"
   End If

   Gbdriveerror = Driveinit()
   If Gbdriveerror = 0 Then
      Reset Watchdog
         If Debugsd = 1 Then
           Print #1 , "Ini File System"
         End If

        ' Select partition 1 (or use 0 for drive without Master Boot Record)
        Errorcode = Initfilesystem(1)
        If Errorcode <> 0 Then
            Print #1 , "Err: " ; Errorcode ; " while initializing file system"
            Print #1 , "Errtext can't be saved at SD"
            Reset Sdinitok
            Reset Watchdog
        Else
         If Debugsd = 1 Then
            Print #1 , "Filesystem type: " ; Gbfilesystem
            Print #1 , "FAT Start Sector: " ; Glfatfirstsector
            Print #1 , "Root Start Sector: " ; Glrootfirstsector
            Print #1 , "Data First Sector: " ; Gldatafirstsector
            Print #1 , "Max. Cluster Nummber: " ; Glmaxclusternumber
            Print #1 , "Sectors per Cluster: " ; Gbsectorspercluster
            Print #1 , "Root Entries: " ; Gwrootentries
            Print #1 , "Sectors per FAT: " ; Glsectorsperfat
            Print #1 , "Number of FATs: " ; Gbnumberoffats
            Print #1 , "Disk size: " ; Disksize() ; " kB"
            Reset Watchdog
         Else
            Print #1 , "SD ok"
         End If
'            Diskinitialized = True
            Set Sdinitok
        End If
   Else
       Print #1 , "Err en Drive Init: " ; Gbdriveerror
       Print #1 , "Errtext can't be saved at SD"
       Reset Sdinitok
   End If
   Print #1 , ""
   Reset Watchdog
End Sub


Sub Versd()
   Do
      Call Diskinsertion()
      If Sdinitok = 0 Then
         Wait 1
         Incr Tmpb
         'Reset Watchdog
      End If
   Loop Until Sdinitok = 1 Or Tmpb = 3
End Sub

'*******************************************************************************
' Busca directorio en raiz de SD
'*******************************************************************************
Sub Verdir()
   Call Diskinsertion()
   If Sdinitok = 1 Then
      Print #1 , "Buscando Directorio " ; Tmpstr8
      Chdir "\"                                             ' Para ir al directorio raiz
      Dirstr = Dir(tmpstr8)
      Tmpw = 0
      If Len(dirstr) > 0 Then
         Tmpstr8 = Filedatetime(dirstr)
         Print #1 , Dirstr ; "," ; Filelen(dirstr) ; "," ; Tmpstr8
         Reset Watchdog
         Dirstr = Dir()
         Incr Tmpw
      Else
         Print #1 , "DIR no encontrado"
      End If
   Else
      Print #1 , "ERROR SD"

   End If
End Sub


Sub Writesd(byval Archivo As String , Texto As String * 255 , Byval Directorio As String * 12 )
   Local Sampl As Byte
   Local Lonok As Byte
   Local Cntrptr As Byte
   'Local Ff As Byte

   Disable Urxc
   Disable Urxc1
   Call Diskinsertion()
   If Sdinitok = 1 Then
      Chdir "\"
      If Directorio <> "\" Then
         Chdir Directorio
      End If
      Ff = Freefile()
      Open Archivo For Append As #ff
      Ptrsd = Lof(#ff)
      Ptrsd1 = Lof(#ff)
      Lonok = 0
      Cntrptr = 0
      Do
         If Ptrsd = Ptrsd1 Then
            Lonok = 1
         Else
            Ptrsd = Lof(#ff)
            Ptrsd1 = Lof(#ff)
         End If
         incr Cntrptr
      Loop Until Lonok = 1 Or Cntrptr = 10
      Get #ff , Sampl , Ptrsd
      Print #ff , Texto
      Close #ff
   Else
      Print #1 , "SD ERR"
   End If
   Enable Urxc
   Enable Urxc1
End Sub

'*******************************************************************************
' Subrutina imprimir log de memoria SD
'*******************************************************************************

Sub Printlogsd()
   'Print #1 , "LOG " ; Cmdsplit(3) ; " a " ; Date$ ; "," ; Time$
   Disable Urxc
   Disable Urxc1
   Call Diskinsertion()
   If Sdinitok = 1 Then
      Chdir "\"
      If Tmpdir <> "\" Then
         Chdir Tmpdir
      End If
      Dirstr = Cmdsplit(2)
      Ff = Freefile()
      Open Dirstr For Binary As #ff
      Tmpl2 = Lof(#ff)
      Tmps = Tmpl2 / 11520
      Print #1 , "Por favor espere " ; Fusing(tmps , "#.#") ; " seg aproximadamente"
      Print #1 , "FILE <" ; Dirstr ; ">, LOF=" ; Tmpl2
      Print #1 , "$INI"
       Do
         Atsnd = ""
         Line Input #ff , Atsnd
         Print #1 , Atsnd
         Reset Watchdog
        Loop Until Eof(#ff) <> 0
      Close #ff
      Print #1 , "$FIN"
   Else
      Cmderr = 9
   End If
   Enable Urxc
   Enable Urxc1

End Sub

'*******************************************************************************
' RTC
'*******************************************************************************
Sub Getdatetimeds3231()

  I2cstart
  Errorrtc = 1                                              ' Generate start code
  If Err = 0 Then
     I2cwbyte Ds3231w                                       ' send address
     I2cwbyte 0                                             ' start address in 1307
     I2cstart                                               ' Generate start code
     If Err = 0 Then
        'Errorrtc = 0
        I2cwbyte Ds3231r                                    ' send address
        'I2crbyte _sec , Ack
        I2crbyte Timebuf(1) , Ack
        'I2crbyte _min , Ack       ' MINUTES
        I2crbyte Timebuf(2) , Ack                           ' MINUTES
        'I2crbyte _hour , Ack                                ' Hours
        I2crbyte Timebuf(3) , Ack                           ' Hours
        I2crbyte Dow , Ack                                  ' Day of Week
        'I2crbyte _day , Ack                                       ' Day of Month
        I2crbyte Datebuf(1) , Ack                           ' Day of Month
        'I2crbyte _month , Ack       ' Month of Year
        I2crbyte Datebuf(2) , Ack                           ' Month of Year
        'I2crbyte _year , Nack       ' Year
        I2crbyte Datebuf(3) , Nack                          ' Year
        I2cstop
        If Err <> 0 Then
         Call Error(15)
        Else
           '_sec = Makedec(_sec) : _min = Makedec(_min) : _hour = Makedec(_hour)
           '_day = Makedec(_day) : _month = Makedec(_month) : _year = Makedec(_year)
           Timebuf(1) = Makedec(timebuf(1))
           Timebuf(2) = Makedec(timebuf(2))
           Timebuf(3) = Makedec(timebuf(3))
           Horatmp = Time(timebuf(1))
           Print #1 , "T_DS3231=" ; Horatmp
           Datebuf(1) = Makedec(datebuf(1))
           Datebuf(2) = Makedec(datebuf(2))
           Datebuf(3) = Makedec(datebuf(3))
           Fechatmp = Date(datebuf(1))
           Print #1 , "F_DS3231=" ; Fechatmp
           Tmpl = Syssec(horatmp , Fechatmp)
           Print #1 , "Hmin=" ; Tmpl
           If Tmpl > Horamin Then
              Time$ = Time(tmpl)
              Date$ = Date(tmpl)
              Errorrtc = 0
           Else
              Print #1 , "Hora no val"
           End If

        End If
     Else
      Print #1 , "No se encontro DS3231 en Getdatetime 2"
     End If
  Else
   Print #1 , "No se encontro DS3231 en Getdatetime 1"
  End If
End Sub

'-----------------------
Sub Setdateds3231()
  I2cstart                                                  ' Generate start code
  If Err = 0 Then
     _day = Makebcd(_day) : _month = Makebcd(_month) : _year = Makebcd(_year)
     I2cwbyte Ds3231w                                       ' send address
     I2cwbyte 3                                                ' starting address in 1307
     I2cwbyte Dow
     I2cwbyte _day                                             ' Send Data to day
     I2cwbyte _month       ' Month
     I2cwbyte _year       ' Year
     I2cstop
     If Err <> 0 Then call Error(15)
  Else
   Print #1 , "No se encontro DS3231 en Setdate"
  End If
end sub
'-----------------------
Sub Settimeds3231()
  I2cstart                                                  ' Generate start code
  If Err = 0 Then
     _sec = Makebcd(_sec) : _min = Makebcd(_min) : _hour = Makebcd(_hour)
     I2cwbyte Ds3231w                                       ' send address
     I2cwbyte 0                                                ' starting address in 1307
     I2cwbyte _sec                                             ' Send Data to SECONDS
     I2cwbyte _min                                             ' MINUTES
     I2cwbyte _hour                                         ' Hours
     I2cstop
     If Err <> 0 Then call Error(15)
  Else
   Print #1 , "No se encontro DS3231 en Settime"
  End If
 end sub
 '----------------------

 '********définition des erreurs***********************************************
Sub Error(byval Genre As Byte )
   Local Mes_error As String * 20
   Select Case Genre
      Case 1
      Mes_error = " Reset  "
      Case 2
      Mes_error = " DFH "
      Case 3
      Mes_error = "set params  "
      Case 4
      Mes_error = "start "
     Case 5
      Mes_error = "Hardstop "
      Case 6
      Mes_error = "Status "
      Case 7
      Mes_error = "Getposition "
      Case 8
      Mes_error = "pas de module"
      Case 9
      Mes_error = "9xx"
      Case 10
      Mes_error = "10xx"
      Case 11
      Mes_error = "11xx"
      Case 12
      Mes_error = "12xx"
      Case 13
      Mes_error = "13xx"
      Case 14
      Mes_error = "ecriture clock"
      Case 15
      Mes_error = "lecture clock"
      Case Else
       Mes_error = "Autre erreur"
   End Select
   Print #1 , "error=" ; Mes_error                             '; Adr_ax
End Sub

'Configuracion de netradas no utilizadas para modeo POWERDOWN
Sub Pwrinpset()
'   Config Portb.0 = Input
'   Set Portb.0
   Config Portc.2 = Input
   Set Portc.2
   Config Porta.3 = Input
   Set Porta.3
   Config Porta.4 = Input
   Set Porta.4
   Config Porta.5 = Input
   Set Porta.5
   Config Porta.6 = Input
   Set Porta.6
   Didr0 = Bits(ain1d , Ain0d)                              'Disable digital input buffer on the AIN1/0 pin
   Set Acsr.acd

   Stop Ac
End Sub


'*******************************************************************************
' Subrutina para leer datos de los canales ADC
'*******************************************************************************

Sub Leeradc()
   For Adcn = 1 To Numadc
      Adct = Lookup(adcn , Tbl_adc)
      Tmpwadc = Getadc(adct)                                'Valor instantaneo
      Adccntri(adcn) = Adccntri(adcn) + Tmpwadc
   Next

   Incr Cntrsmpl
   Cntrsmpl = Cntrsmpl Mod Numsample
   If Cntrsmpl = 0 Then
      Set Smplrdy
      For Adcn = 1 To Numadc
         Adc_data(adcn) = Adccntri(adcn) / Numsample
         Adc_data(adcn) = Adc_data(adcn) * Kadc(adcn)
         If Enabug.1 = 1 Then
            Print #1 , Adcn ; "=" ; Fusing(adc_data(adcn) , "#.##")
         End If
         Adccntri(adcn) = 0
      Next
      Vmain = Adc_data(1)                                   '+ 0.43
      Vmdc = Adc_data(2)                                    '+ 0.43
   End If

End Sub

Sub Leer1864()
   Set Conv
   Waitus 5
   Reset Conv
   Shiftin Sdin , Sck , Tmpwadc , 1
   'Print #1 , Hex(tmpwadc) ; "," ; Tmpwadc
End Sub

Sub Leercanal(byval Canal As Byte)
   Local Tmpcanal As Byte
   Tmpcanal = Canal - 1
   If Enacan.tmpcanal = 1 Then
      Sela = Tmpcanal.0
      Selb = Tmpcanal.1
   End If
   Reset Ena
   Waitus 100
   Call Leer1864()
   Valwadc(canal) = Tmpwadc
   If Initstadc = 0 Then
      Set Ena
   Else
      Reset Ena
   End If
   Waitus 100
End Sub

Sub Leerinadc()
      'Set Pindbg
      Incr Cntrsamples
      If Enabug.0 = 1 Then
         Print #1 , "Sample=" ; Cntrsamples ; "," ; Time$
      End If

      For Tmpb = 1 To Numcan
         Tmpb2 = Tmpb - 1
         If Enacan.tmpb2 = 1 Then
            Call Leercanal(tmpb)
'            Ptrcanal = Tmpb2 * Nums
'            Ptrcanal = Ptrcanal + Cntrsamples
            Can_val(tmpb) = Can_val(tmpb) + Valwadc(tmpb)
            If Enabug.2 = 1 Then
               Print #1 , "ValHEX(" ; Tmpb ; ")=" ; Valwadc(tmpb);
               Tmps = Mval(tmpb) * Valwadc(tmpb)
               Tmps = Tmps * Kcal(tmpb)
               Tmps = Tmps + Bval(tmpb)
               Print #1 , ",Valcan=" ; Fusing(tmps , "#.##") ; ",";
            End If
         End If
         If Enabug.2 = 1 Then
            Print #1,
         End If
      Next

      Cntrsamples = Cntrsamples Mod Numsampleb
      If Cntrsamples = 0 Then
         Set Samplerdy
         For Tmpb = 1 To Numcan
            Tmpb2 = Tmpb - 1
            If Enacan.tmpb2 = 1 Then
               If Enabug.3 = 1 Then
                  Print #1 , "Canal " ; Tmpb ; "," ; Time$ ; ",";
               End If
               Tmps = Can_val(tmpb) / Numsampleb
               Tmps = Mval(tmpb) * Valwadc(tmpb)
               Tmps = Tmps * Kcal(tmpb)
               Tmps = Tmps + Bval(tmpb)
               Can_vals(tmpb) = Tmps
               If Enabug.3 = 1 Then
                  Print #1 , "Phex=" ; Can_val(tmpb) ; ",P=" ; Can_vals(tmpb)
               End If
               Can_val(tmpb) = 0
            End If
         Next
      End If

End Sub



Sub Savetxini()
   Print #1 , "Verif " ; Filenotx
   Idest = Idesteep
   'Chdir "\"
   'Chdir "NOTX"
   'Tmpstr52 = "NO_INI.TXT"
   Tmpstr52 = Filenotx
   Dirstr = Dir(tmpstr52)
   If Len(dirstr) > 0 Then
      Print #1 , "Borro archivo " ; Tmpstr52
      Chdir "\"
      'Chdir "NOTX"
      Chdir Tmpdir
      Ff = Freefile()
      Open Tmpstr52 For Binary As #ff
      Close #ff
      Kill Tmpstr52
      Print #1 , "Err code:" ; Gbdoserror
      Flush #ff
      Atsnd = "Borro archivo " + Tmpstr52
      Call Writesd( "EV_LOG.TXT" , Atsnd , "LOG")
   End If

   'Filenotx="NO_INI.TXT"
   Print #1 , "TX ini en " ; Filenotx
   Fechatmp = Date$
   Horatmp = Time$
   Atsnd = "$" + Idest + ",0021," + Fechatmp + "," + Horatmp + ",TC;"
   Tmpstr52 = Version(3)
   Atsnd = Atsnd + Tmpstr52 + " "
   Tmpstr52 = Version(1)
   Atsnd = Atsnd + Tmpstr52 + ";" + Str(tadq) + ";" + Str(tac)
   For Tmpb = 1 To Numtxaut
      Tmpstr52 = Time(autoval(tmpb))
      Atsnd = Atsnd + ";" + Tmpstr52
   Next
   Print #1 , Atsnd
   'Call Writesd(filenotx , Atsnd , "NOTX")
   Call Writesd(filenotx , Atsnd , Tmpdir)
   Print #1 , "TX oper"
   Atsnd = "$" + Idest + ",0022," + Fechatmp + "," + Horatmp + ",TO;"
   Atsnd = Atsnd + Str(cntrini) + ";" + Str(cntrtramas) + ";" + Mopstr + ";"
   Atsnd = Atsnd + Imei + ";" + Idserial + ";" + Numserialwtch + ";" + Estadogps + ";" + Latitud + " " + Longitud
   Print #1 , Atsnd
   'Call Writesd(filenotx , Atsnd , "NOTX")
   Call Writesd(filenotx , Atsnd , Tmpdir)
   Print #1 , "TX srv"
   Atsnd = "$" + Idest + ",0023," + Fechatmp + "," + Horatmp + ",TS;"
   Atsnd = Atsnd + Iptx + ";" + Ptotx + ";" + Apn
   Print #1 , Atsnd
   'Call Writesd(filenotx , Atsnd , "NOTX")
   Call Writesd(filenotx , Atsnd , Tmpdir)
   Print #1 , "TX tlf"
   Atsnd = "$" + Idest + ",0024," + Fechatmp + "," + Horatmp + ",TT;"
   Atsnd = Atsnd + Tlf(1) + ";" + Tlf(2) + ";" + Tlf(3) + ";" + Tlf(4)
   Print #1 , Atsnd
   'Call Writesd(filenotx , Atsnd , "NOTX")
   Call Writesd(filenotx , Atsnd , Tmpdir)

   For Tmpb = 1 To Numcan
      Tmpb2 = Tmpb - 1
      Print #1 , "TX C" ; Tmpb
      Atsnd = "$" + Idest + ",003" + Str(tmpb) + "," + Fechatmp + "," + Horatmp + ",TCanal" + Str(tmpb) + ";"
      Atsnd = Atsnd + Str(enacan.tmpb2) + ";" + Tipocan(tmpb) + ";" + Idcan(tmpb)
      Atsnd = Atsnd + ";" + Str(iniescala(tmpb)) + ";" + Str(fonescala(tmpb))
      Atsnd = Atsnd + ";" + Str(ndcan(tmpb)) + ";" + Unican(tmpb)
      Print #1 , Atsnd
      'Call Writesd(filenotx , Atsnd , "NOTX")
      Call Writesd(filenotx , Atsnd , Tmpdir)
   Next

   Print #1 , "TX C5"
   Atsnd = "$" + Idest + ",0035" + "," + Fechatmp + "," + Horatmp + ",TCanal5" + ";"
   Atsnd = Atsnd + "1" + ";" + "V" + ";" + "MB1"
   Atsnd = Atsnd + ";" + "0" + ";" + "5"
   Atsnd = Atsnd + ";" + "2" + ";" + "V"
   Print #1 , Atsnd
   'Call Writesd(filenotx , Atsnd , "NOTX")
   Call Writesd(filenotx , Atsnd , Tmpdir)

   Print #1 , "TX C6"
   Atsnd = "$" + Idest + ",0036" + "," + Fechatmp + "," + Horatmp + ",TCanal6" + ";"
   Atsnd = Atsnd + "1" + ";" + "V" + ";" + "MB2"
   Atsnd = Atsnd + ";" + "0" + ";" + "5"
   Atsnd = Atsnd + ";" + "2" + ";" + "V"
   Print #1 , Atsnd
   'Call Writesd(filenotx , Atsnd , "NOTX")
   Call Writesd(filenotx , Atsnd , Tmpdir)

   Print #1 , "TX C7"
   Atsnd = "$" + Idest + ",0037" + "," + Fechatmp + "," + Horatmp + ",TCanal7" + ";"
   Atsnd = Atsnd + "1" + ";" + "V" + ";" + "MB3"
   Atsnd = Atsnd + ";" + "0" + ";" + "5"
   Atsnd = Atsnd + ";" + "2" + ";" + "V"
   Print #1 , Atsnd
   'Call Writesd(filenotx , Atsnd , "NOTX")
   Call Writesd(filenotx , Atsnd , Tmpdir)

   Print #1 , "TX C8"
   Atsnd = "$" + Idest + ",0038" + "," + Fechatmp + "," + Horatmp + ",TCanal8" + ";"
   Atsnd = Atsnd + "1" + ";" + "V" + ";" + "GSM"
   Atsnd = Atsnd + ";" + "0" + ";" + "20"
   Atsnd = Atsnd + ";" + "2" + ";" + "dB"
   Print #1 , Atsnd
   'Call Writesd(filenotx , Atsnd , "NOTX")
   Call Writesd(filenotx , Atsnd , Tmpdir)

'   Incr Ptrnotx
'   Ptrnotxeep = Ptrnotx
'   Filenotx = "NO_" + Str(ptrnotx) + ".TXT"

End Sub


Sub Savetx()
   Kw = Ptrsavedat + 1
   Txdattime(kw) = Syssec()
   Txdatcan1(kw) = Can_vals(1)
   Txdatcan2(kw) = Can_vals(2)
   Txdatcan3(kw) = Can_vals(3)
   Txdatcan4(kw) = Can_vals(4)
   Txdatbat(kw) = Vmain
   Txdatvmdc(kw) = Vmdc
   Gsmdat(kw) = Val(gsmval)
   'Txdatcntrini(kw) = Cntrini
   Txbuf(kw) = 1
   Call Gentrama()
   Call Writesd(filelog , Tmpstr52 , "DATOS")

   Ptrsavedat = Kw Mod Nbuf
End Sub

Sub Txaut()
   Kw = Ptrtx

   Call Gentrama()
   Tramatmp = Tmpstr52
   Tmpstr52 = Tmpstr52 + "#" + Iptx + "#" + Ptotx
   Print #1 , Tmpstr52
   Print #2 , Tmpstr52
   Txbuf(kw) = 0
   'Txcntrdat = Txcntrdat - 1
   'Ptrtxdat = Kw Mod Nbuf
   Numpar = Split(tmpstr52 , Cmdsplit(1) , ",")
   If Numpar > 0 Then
      Idtxtrama = Cmdsplit(2) + "," + Cmdsplit(3)
   End If

End Sub


Sub Gentrama()
   Idest = Idesteep
   Tmpstr52 = "$" + Idest + ","
   Tmpl = Txdattime(kw)
   Tmpstr8 = Date(tmpl)
   Tmpstr52 = Tmpstr52 + Tmpstr8 + ","
   Tmpstr8 = Time(tmpl)
   Tmpstr52 = Tmpstr52 + Tmpstr8
'   Tmpstr52 = Tmpstr52                                      '+ ","                                '+ Daa_keyapi
   For Tmpb = 1 To Numcan
      Tmpb2 = Tmpb - 1
      If Enacan.tmpb2 = 1 Then
         Select Case Tmpb
            Case 1:
               Tmps = Txdatcan1(kw)
               If Tmps < 0 Then
                  TMPS=0
               End If
            Case 2:
               Tmps = Txdatcan2(kw)
            Case 3:
               Tmps = Txdatcan3(kw)
            Case 4:
               Tmps = Txdatcan4(kw)
         End Select

         Select Case Ndcan(tmpb)
            Case 1:
               Tmpfusing = Fusing(tmps , "#.#")
            Case 2:
               Tmpfusing = Fusing(tmps , "#.##")
            Case 3:
               Tmpfusing = Fusing(tmps , "#.###")
            Case 4:
               Tmpfusing = Fusing(tmps , "#.####")
            Case 5:
               Tmpfusing = Fusing(tmps , "#.#####")
            Case 6:
               Tmpfusing = Fusing(tmps , "#.######")
            Case 7:
               Tmpfusing = Fusing(tmps , "#.#######")
            Case 8:
               Tmpfusing = Fusing(tmps , "#.########")
            Case 9:
               Tmpfusing = Fusing(tmps , "#.#########")
            Case Else
               Tmpfusing = Fusing(tmps , "#.##")
         End Select
         Tmpstr52 = Tmpstr52 + "," + Idcan(tmpb) + "," + Tmpfusing
      Else
         Tmpstr52 = Tmpstr52 + "," + Idcan(tmpb) + ","
      End If
   Next
   'Tmpstr52 = Tmpstr52 + "," + "MB1," + Fusing(txdatbat(kw) , "#.##") + "," + "MB2," + Fusing(txdatvmdc(kw) , "#.##") + ",MB3," + Fusing(txdatbat(kw) , "#.##") + ",GSM," + Str(gsmdat(kw)) + ",D"
   Tmpstr52 = Tmpstr52 + "," + "MB1," + Regmdb(1) + "," + "MB2," + Regmdb(2) + ",MB3," + Regmdb(3) + ",GSM," + Str(gsmdat(kw)) + ",D"


End Sub


Sub Defaultvalues()
   'Idesteep = "WAGGER"
   Tadqeep = 5
   Taceep = 300
   Trtxeep = 120
   For Tmpb = 1 To Numcan
      Tmpstr52 = ""
      Tmpstr52 = "CAN" + Str(tmpb)
      Idcaneep(tmpb) = Tmpstr52
      Iniescalaeep(tmpb) = 4
      Fonescalaeep(tmpb) = 20
      Kcaleep(tmpb) = 1.709953
      Ndcaneep(tmpb) = 3
   Next
   Idcaneep(1) = "N1"
   Idcaneep(2) = "Q1"
   Idcaneep(3) = "CNO"
   Idcaneep(4) = "CNO"
   Tipocaneep(1) = "N"
   Tipocaneep(2) = "Q"
   Tipocaneep(3) = "CNO"
   Tipocaneep(4) = "CNO"
   Unicaneep(1) = "m"
   Unicaneep(2) = "l/s"
   Unicaneep(3) = "nd"
   Unicaneep(4) = "nd"

   Enacaneep = &H03
   Kadceep(1) = 0.005388196
   Kadceep(2) = 0.005388196
   Cntrinieep = 0
   Cntrtramaseep = 0

   Tmpstr52 = "07:00:00"
   Tmpl = Secofday(tmpstr52)
   Autovaleep(1) = Tmpl

   Tmpstr52 = "17:00:00"
   Tmpl = Secofday(tmpstr52)
   Autovaleep(2) = Tmpl

   Tmpstr52 = "08:00:00"
   Tmpl = Secofday(tmpstr52)
   Autovaleep(3) = Tmpl

   Tmpstr52 = "09:00:00"
   Tmpl = Secofday(tmpstr52)
   Autovaleep(4) = Tmpl

   Ptrnotxeep = 0

   For Tmpb = 1 To Numtlf
      Tlfeep(tmpb) = "0999988873"
   Next

#if Modoproduc = 0
   Iptxeep = "181.199.76.185"
   Ptotxeep = "3000"
   Apneep = "internet.claro.com.ec"
#endif

#if Modoproduc = 1
   Iptxeep = "10.1.1.127"
   Ptotxeep = "3000"
   Apneep = "emaap.internet"
#endif


   Enaautoeep = &B00000011
   Idserialeep = "IDSER"
   Numserialwtcheep = "W-XXX"
'   Idest = "W-XXXX"
   Latitudeep = "-0.20276"
   Longitudeep = "-78.49181"
   Estadogpseep = "V"
   Set Inivariables

End Sub

'*******************************************************************************
' Procesamiento de datos MDC
'*******************************************************************************
Sub Procmdc()
   Numpar = Split(serproc1 , Cmdsplit(1) , ";")
   If Numpar > 0 Then
      For Tmpb = 1 To Numpar
         Print #1 , Tmpb ; ":" ; Cmdsplit(tmpb)
      Next
   End If
   Atsnd = Serproc1 + "," + Time$ + "," + Date$

   If Len(cmdsplit(1)) = 6 Then
      Cmdtmp = Cmdsplit(1)
      Cmdtmp = Ucase(cmdtmp)
      Cmderr = 255
      Reset Newcmdmdc
      Select Case Cmdtmp
         Case "SETMBD"
            Tmpstr52 = Cmdsplit(2)
            Fonorxmdc = Cmdsplit(3)
            Fecharxmdc = Cmdsplit(4)
            Set Newcmdmdc
            Call Writesd( "MDC_LOG.TXT" , Atsnd , "LOG")

         Case "TXTRAM"
            Tmpstr52 = Cmdsplit(2)
            Idrxtrama = Cmdsplit(3)
            If Cmdsplit(2) = "OK" Then
               Print #1 , "TRAMA OK," ; Idrxtrama
            End If
            If Cmdsplit(2) = "ERROR" Then
               Print #1 , "TRAMA ERROR," ; Idrxtrama
            End If
            Txtrama = Cmdsplit(2)
            Set Proctramfile
            Reset Iniwaitrx
            Cntrwaitrx = 0

         Case "TXFILE"
            Tmpstr52 = Cmdsplit(2)
            Idrxfile = Cmdsplit(3)
            If Cmdsplit(2) = "OK" Then
               Print #1 , "FILE OK," ; Idrxfile
            End If
            If Cmdsplit(2) = "ERROR" Then
               Print #1 , "FILE ERROR," ; Idrxfile
            End If
            Txtrama = Cmdsplit(2)
            Set Proctramfile
            Reset Iniwaitrx
            Cntrwaitrx = 0

         Case "LEESER"
            If Numpar = 2 Then
               Idserial = Cmdsplit(2)
               Idserialeep = Idserial
            Else
               Print #1 , "Numser no val"
            End If

         Case "LEEGSN"
            If Numpar = 2 Then
               Imei = Cmdsplit(2)
               Imeieep = Imei
            Else
               Print #1 , "Numser no val"
            End If

         Case "LEEGSM"
            If Numpar = 2 Then
               Gsmval = Cmdsplit(2)
            Else
               Print #1 , "GSM NO VAL"
            End If

      End Select


   End If

   If Newcmdmdc = 1 Then
      Idest = Idesteep
      Reset Newcmdmdc
      Serproc = Tmpstr52
      Call Procser()
      Tramatmp = Atsnd
      Fechatmp = Date$
      Horatmp = Time$
      Atsnd = "$" + Idest + ",1000," + Fechatmp + "," + Horatmp + ","
      Atsnd = Atsnd + Tramatmp + "," + Fonorxmdc
      Incr Ptrnotx
      Ptrnotxeep = Ptrnotx
      Filenotx = "NO_" + Str(ptrnotx) + ".TXT"
      Call Writesd(filenotx , Atsnd , "NOTX")
      Tmpstr52 = Atsnd + "&" + Fonorxmdc
      Print #1 , Tmpstr52
      Print #2 , Tmpstr52
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


   'Call Writesd( "MDC_LOG.TXT" , Atsnd , "LOG")

End Sub

'*******************************************************************************
'TABLA DE DATOS
'*******************************************************************************
Tbl_adc:
Data 0                                                      'dummy data
Data 0
Data 1
Data 2
Data 3
Data 4
Data 5
Data 6

Tbl_err:
Data "OK"                                                   '0
Data "Comando no reconocido"                                '1
Data "Longitud comando no valida"                           '2
Data "Numero de usuario no valido"                          '3
Data "Numero de parametros invalido"                        '4
Data "Error longitud parametro 1"                           '5
Data "Error longitud parametro 2"                           '6
Data "Parametro no valido"                                  '7
Data "ERROR8"                                               '8
Data "ERROR SD. Intente de nuevo"                           '9

Tabla_estado:
Data &B00000000000000000000000000000000&                    'Estado 0
Data &B00000000000000000000000000000011&                    'Estado 1
Data &B00000000000000000000000000110011&                    'Estado 2
Data &B00000000000000000000001100110011&                    'Estado 3
Data &B00000000000000000011001100110011&                    'Estado 4
Data &B00000000000000110011001100110011&                    'Estado 5
Data &B00000000000011001100000000110011&                    'Estado 6
Data &B00001111111111110000111111111111&                    'Estado 7
Data &B01010101010101010101010101010101&                    'Estado 8
Data &B00110011001100110011001100110011&                    'Estado 9
Data &B01110111011101110111011101110111&                    'Estado 10
Data &B11111111111111000000000000001100&                    'Estado 11
Data &B11111111111111000000000011001100&                    'Estado 12
Data &B11111111111111000000110011001100&                    'Estado 13
Data &B11111111111111001100110011001100&                    'Estado 14
Data &B11111111111111000000000000001100&                    'Estado 15
Data &B11111111111111111111111111110000&                    'Estado 16



Loaded_arch: