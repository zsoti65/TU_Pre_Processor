Winapp

PROGRAM MainProgram

! ITU Driver version 

  use TU_V1M1J18, only : Systemfontname,SystemFontSize,TuSecMax,rwper,rb_oneFile,TuUnit,TuUnit0,clientversion,ttInfo
  
  use clrwin
  
  implicit none

  external TuEditV1M1J18Drv
  
  integer ip,TuEditV1M1J18Drv                      
 
  Systemfontname = 'Arial'     ! set font
  SystemFontSize = 1.0D+00     ! set font size
  
  TuSecMax       = 10          ! init for number of sections used by default
  rwper          =  1          ! enable write permission
  rb_oneFile     =  0          ! multiple datasets in one file is off
  TuUnit         = 25          ! output unit
  TuUnit0        = 26          ! scratch unit

  clientversion  = 0           ! this is a standalone version
  
  ip = ttInfo('Welcome','Welcome to the Transuranus Input Editor' //char(10)//char(10)// &
              'Please notice:'//char(10)// &
              'Use this software on your own liability and risk.'//char(10)// &
              'The author disclaims any liability for functionality and results.'//char(10)//char(10)//&
              'Known restrictions:'//char(10)// &
              'Existing input files must not have comment lines in between datasets with line wraps!'//char(10)// &
              'This may lead to an error while reading input files.'//char(10)//char(10)//& 
              'Version 1.1 for the Transuranus version V1M1J18'//char(10)// & 
              'Last modification: October 2019'//char(10)//char(10)// & 
                      'G. Spykman, TUV NORD EnSys GmbH & Co.KG')
     
  ip = TuEditV1M1J18Drv()

  

end program

! -----------------------------------------------------------------------------

Integer function TuEditV1M1J18Drv()

  use clrwin
  use TU_V1M1J18
  
!  use TITANIA_Module_LOCA, only : ApplyLoca
!  use TITANIA_Module_Stat, only : ApplyStat

  use Dummy_Module, only : ApplyLoca, ApplyStat, Distrib

  
  CHARACTER (LEN=40)::FDATE@
  
  integer*2 Handle,FileError
  integer   nbytesread
  
  iprog = TuInputDefaults()
  iprog = InitTUInputPlot()
  
  TuHandbookPath = '.\'
  projectpath    = '.\'
  
  call openr@('TUInputEditor.ini',Handle,FileError)
  if (FileError == 0) then
    do 
      call readfa@(chdummy1,Handle,nbytesread,FileError)
      if (FileError /= 0 .or. nbytesread < 0) exit
      
      select case (chdummy1(1:nbytesread))
      
        case('#handbook path')
          call readfa@(TuHandbookPath,Handle,nbytesread,FileError)
          if (FileError /= 0 .or. nbytesread < 0) exit
          
        case('#project path')
          call readfa@(projectpath,Handle,nbytesread,FileError)
          if (FileError /= 0 .or. nbytesread < 0) exit    

      end select

    end do 

    call closef@(Handle,FileError)      

  end if
  
  TuEditName = trim(projectpath)//'Transuranus.input'
  TuName     = trim(projectpath)//'input'
  
  TuEditName = ''
  TuName     = ''

  TD%version = FDATE@() 
  
  TD%tobesaved = 0
  TW  = TD
  TWM = TDM
  
  iprog = IblocIstruk()
  iprog = ModProp()
  iprog = CheckRadialCoarseZones()
  iprog = KoKoKo()
  iprog = setigd()
  
  i=winio@('%mi[CorePic]&')
  i=winio@('%ww[thin_border]%ca@&',trim(CaptionStr))
  
  i=winio@('%mn[&File[~&New Dataset]]&',RWPer,NewTuData)
  i=winio@('%mn[[|,&Load Dataset]]&','FILE_OPENR[Load editor data file]',TuEditName,LoadTuData)
  i=winio@('%mn[[~&Save Dataset]]&',RWPer,'FILE_OPENW[Save editor data]',TuEditName,SaveTuData)
  i=winio@('%mn[[|,Load H&eader data]]&','FILE_OPENR[Load editor data file]',TuEditName,LoadTuData2)
  i=winio@('%mn[[~Save &Header data only]]&',RWPer,'FILE_OPENW[Save editor data]',TuEditName,SaveTuData2)
  i=winio@('%mn[[|,Se&ttings]]&',EditSettings)
  i=winio@('%mn[[|,E&xit]]&',ExitTuData) 
  
  i=winio@('%mn[&Transuranus Input[~&Read Input File]]&',RWPer,'FILE_OPENR[Read TU input file]',TuName,ReadTuInputGeo)
  i=winio@('%mn[[&Create Input File]]&','FILE_OPENW[Create TU input file]',TuName,InputTuData)
  i=winio@('%mn[[&Display Input]]&',ViewTuData)
  
  i=winio@('%mn[Exten&sions[~&Apply to TITANIA]]&',clientversion,ApplyTuData)
  i=winio@('%mn[[~Add Thermoh&ydraulic Data]]&',clientversion,'FILE_OPENR[Add thermo hydraulic data set]',TuName,ReadThData) 
  i=winio@('%mn[[~TNEH Analysis[LOCA]]]&',clientversion,ApplyLoca) 
  i=winio@('%mn[[[~Probabilistic]]]&',clientversion,ApplyStat)   
  
  i=winio@('%mn[&Help[Transuranus Input Editor]]&',HelpMainScreen)
  i=winio@('%mn[[&Transuranus Handbook]]&',OpenTuHandbook)
 
  i=winio@('%mn[E&xit]&',ExitTuData) 
   
    i=winio@('%2.1ob[no_border]&')
    
      i=winio@('%3.19ob[no_border]&')
 
      i=winio@('%cb%20^bt[Input header data]&',TuEdit1)
      i=winio@(' %cb%bfIDEN to ITEXTK %`bf %cb&')
  
      i=winio@('%cb%20^bt[Model selection part 1]&',TuEdit2)
      i=winio@(' %cb%bfM3 to KPLOT %`bf%cb&')
    
      i=winio@('%cb%20^bt[Model selection part 2]&',TuEdit3)
      i=winio@(' %cb%bfIHGAP to KOKOKO %`bf%cb&')
  
      i=winio@('%cb%20^bt[Model selection part 3]&',TuEdit4)
      i=winio@(' %cb%bfISLICE to IHUPTK %`bf%cb&')
      
      if (clientversion ==1) then
        i=winio@('%cb%20^bt[Model selection part 4]&',TuEdit4b)
        i=winio@(' %cb%bfIPHASEZR to ITuNoOutput %`bf%cb&')
      else
        i=winio@('%cb%20^bt[Model selection part 4]&',TuEdit4b)
        i=winio@(' %cb%bfIPHASEZR to iHBSLocaRel %`bf%cb&')
      end if 
  
      i=winio@('%cb%20~^bt[Model selection part 5]&',TuInpGrey(1),TuEdit5)
      i=winio@(' %cb%bfCoolant blockage (ibloc=1) and Structure (istruk=1) %`bf%cb&')
  
      i=winio@('%cb%20^bt[Generals material properties]&',TuEdit6)
      i=winio@(' %cb%bfGeneral selection of coolant, cladding and fuel properties  %`bf%cb&')

      i=winio@('%cb%20~^bt[Cladding properties]&',TuInpGrey(2),TuEdit7)
      i=winio@(' %cb%bfSelection of cladding properties (ModClad) %`bf%cb&')

      i=winio@('%cb%20~^bt[Fuel properties]&',TuInpGrey(3),TuEdit8)
      i=winio@(' %cb%bfSelection of fuel properties (ModFuel) %`bf%cb&')
  
      i=winio@('%cb%20~^bt[Coolant properties]&',TuInpGrey(4),TuEdit9)
      i=winio@(' %cb%bfSelection of coolant properties (ModCool) %`bf%cb&')

      if (clientversion ==1) then

        i=winio@('%cb%20~^bt[TUEV modifications]&',TW%iTuevMod,TuEditTuevMod)
        i=winio@(' %cb%bfSelection of TUEV modifications (TuevMod_1,2) %`bf%cb&')
      
      else
        
        i=winio@('%cb%cb%cb&')
      
      end if  
      
      i=winio@('%cb%20~^bt[Probabilistic analysis]&',TW%iStati,TuEditStati)
      i=winio@(' %cb%bfMonte Carlo statistical analysis %`bf%cb&')
      
      i=winio@('%cb%20~^bt[RIA analysis]&',TW%iRia,TuEditRia)
      i=winio@(' %cb%bfReactivity initiated accident analysis %`bf%cb&')
      
      i=winio@('%cb%20^bt[Modelling data 1]&',TuEdit10)
      i=winio@(' %cb%bfBETA to RRRR5 %`bf%cb&')
      
      i=winio@('%cb%20^bt[Modelling data 2]&',TuEdit11)
      i=winio@(' %cb%bfFMUEH to P34INP %`bf%cb&')

      i=winio@('%cb%20~^bt[Modelling data 3]&',TuInpGrey(5),TuEdit12)
      i=winio@(' %cb%bfConvergence limits for KOKOKO > 0 %`bf%cb&')

      i=winio@('%cb%20^bt[Modelling data 4]&',TuEdit13)
      i=winio@(' %cb%bfFASTLF to CANF(1..10) %`bf%cb&')
  
      i=winio@('%cb%20~^bt[Model selection LOCA]&',TW%iLoca,TuEditLoca)
      i=winio@(' %cb%bftLOCA to STRLIM %`bf%cb&')
      
      i=winio@('%cb%20~^bt[Cladding properties LOCA]&',TW%iLoca,TuEditLoca2)
      i=winio@(' %cb%bfSelection of LOCA cladding properties (ModClad_LOCA) %`bf%cb&')
  
      i=winio@('%ff%nl%2.3ob[no_border]&')  
      i=winio@('%bfData case id: %`bf%cb&')
      i=winio@('%co[check_on_focus_loss]%^40rs&',TW%DataCaseId,checkNkomm)
      i=winio@('%cb%bfVersion: %`bf%cb%40rs&',TW%Version)
      i=winio@('%cb%bfFuel rod type: %`bf%cb%40rs&',TW%BSType)
      i=winio@('%cb&')
 
    i=winio@('%cb&') 
    
      i=winio@('%3.19ob[no_border]&') 
      
      i=winio@('%cb%20^bt[Modelling data 5]&',TuEdit14)
      i=winio@(' %cb%bfHHREF(1..M31) %`bf%cb&')

      i=winio@('%cb%20^bt[Modelling data 6]&',TuEdit15)
      i=winio@(' %cb%bfAEAX1 to IFALLL(1..MEND) %`bf%cb&')
 
      i=winio@('%cb%20^bt[Modelling data 7]&',TuEdit16)
      i=winio@(' %cb%bfM2(1..IEND1) %`bf%cb&')

      i=winio@('%cb%20~^bt[Modelling data 8]&',TW%istruk,TuEdit17)
      i=winio@(' %cb%bfAESTR, M1STR (coarse zones in structure)%`bf%cb&')

      i=winio@('%cb%20^bt[Modelling data 9]&',TuEdit18)
      i=winio@(' %cb%bfRadii, RAUHL and RAB1 (coarse zones)%`bf%cb&')

      i=winio@('%cb%20^bt[Modelling data 10]&',TuEdit19)
      i=winio@(' %cb%bfRadial fine zones%`bf%cb&')
  
      i=winio@('%cb%20~^bt[Modelling data 11]&',TW%istruk,TuEdit20)
      i=winio@(' %cb%bfRadial zones of structure%`bf%cb&')

      i=winio@('%cb%20^bt[Modelling data 12]&',TuEdit21)
      i=winio@(' %cb%bfKORNGR (incl. radial) and DKORN%`bf%cb&')

      i=winio@('%cb%20^bt[Modelling data 13]&',TuEdit22)
      i=winio@(' %cb%bfPOROSI (incl. radial), PRODIS, OPENPOR and PORHE%`bf%cb&')

      i=winio@('%cb%20~^bt[Modelling data 14]&',TuInpGrey(6),TuEdit23)
      i=winio@(' %cb%bfEnrichment ENRIU235 ...%`bf%cb&')
  
      i=winio@('%cb%20~^bt[Modelling data 15]&',TuInpGrey(8),TuEdit24)
      i=winio@(' %cb%bfIntegrated Fuel Burnable Absorber (IFBA)%`bf%cb&')

      i=winio@('%cb%20~^bt[Modelling data 16]&',TuInpGrey(7),TuEdit25)
      i=winio@(' %cb%bfGadolinium oxide (IGD)%`bf%cb&')

      i=winio@('%cb%20~^bt[Modelling data 17]&',TuInpGrey(6),TuEdit26)
      i=winio@(' %cb%bfPower Density form factor%`bf%cb&')
 
      i=winio@('%cb%20~^bt[Modelling data 18]&',TuInpGrey(6),TuEdit27)
      i=winio@(' %cb%bfStoichiometry: oxygen/metall ratio %`bf%cb&')
      
      i=winio@('%cb%20~^bt[Modelling data 19]&',TuInpGrey(6),TuEdit28)
      i=winio@(' %cb%bfPlutionium concentration (FBR only)%`bf%cb&')
      
      i=winio@('%cb%20~^bt[Modelling data 20]&',TW%ioxide,TuEdit28a)
      i=winio@(' %cb%bfInitial oxide layers%`bf%cb&')

      i=winio@('%cb%20^bt[Modelling data 21]&',TuEdit29)
      i=winio@(' %cb%bfInner pin pressure%`bf%cb&')

      i=winio@('%cb%20^bt[Macro label]&',TuEdit30)
      i=winio@(' %cb%bfIMacro and dummy variables%`bf%cb&')

      i=winio@('%cb%20^bt[Macro data]&',TuEdit31)
      i=winio@(' %cb%bfInput for up to 5000 macro timesteps%`bf%cb&')

      i=winio@('%ff%nl%rb[Display input while reading file]&',LogInputRead)
    
      if (clientversion ==1) then
        i=winio@('%2nlUse menue Extension -> Apply to TITANIA or save data to use not fuel rod type specific data &')
        i=winio@('%nllike radial geometry, mass flow rate etc. in Transuranus-input files created by TITANIA!&') 
      end if  
    
     i=winio@('%cb')  
     
  TuEditV1M1J18Drv = 1
  
end function

! ---------------------------------------------------

resources

CorePic    ICON   C:\Projekte_lokal\Entwicklung\TITANIA.New\Resourcefiles\core1.ICO
