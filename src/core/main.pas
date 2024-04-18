unit main ;

interface
uses sevenzip, SysUtils ;

type
  TMain = class
  private
    compressionlevel:Integer ;
    compressionmethod:TZipCompressionMethod ;
    zipfile:string ;
    function ParseCommand(const s:string; var cmd:string;
      var args:TArray<string>):Boolean ;
    procedure ExitWithError(const msg:string; code:Integer = 1) ;
  public
    procedure Run() ;
  end;

implementation
uses Classes ;

procedure TMain.ExitWithError(const msg: string; code: Integer);
begin
  Writeln(msg) ;
  Halt(code) ;
end;

function TMain.ParseCommand(const s: string; var cmd: string;
  var args: TArray<string>): Boolean;
var arr:TArray<string> ;
    i:Integer ;
begin
  // В текущей реализации не поддерживается парсинг с кавычками - пробелы в именах нельзя
  // Нужно либо сделать чистовой паросер, либо убирать кавычки при возрате строки Split
  arr:=s.Split([' '],'"') ;
  if Length(arr)<2 then Exit(False) ;
  cmd:=arr[0] ;
  SetLength(args,Length(arr)-1) ;
  for i := 0 to Length(arr)-2 do
   args[i]:=arr[i+1] ;
  Result:=True ;
end;

procedure TMain.Run() ;
var arc: I7zOutArchive;
    script:TStringList ;
    s,cmd:string ;
    args:TArray<string> ;
begin
  try
    Writeln('This source is prototype for development purposes only!!!') ;
    if ParamCount<1 then ExitWithError('Usage: scriptfile') ;

    compressionlevel:=5 ;
    compressionmethod:=TZipCompressionMethod.mzDeflate ;
    zipfile:='' ;

    arc := CreateOutArchive(CLSID_CFormatZip);

    if not FileExists(ParamStr(1)) then
      ExitWithError('Not found script file: '+ParamStr(1)) ;

    script:=TStringList.Create() ;
    script.LoadFromFile(ParamStr(1)) ;
    for s in script do begin
      if s.Trim().Length=0 then Continue ;
      if s.Trim().StartsWith('#') then Continue ;
      if not ParseCommand(s,cmd,args) then
        ExitWithError('Error parsing line: '+s) ;
      if cmd='ZipFile' then begin
        zipfile:=args[0] ;
        Writeln('Setting zipfile: '+zipfile) ;
      end;
      if cmd='File' then begin
        if not FileExists(args[0]) then ExitWithError('Not found file: '+args[0]) ;
        if Length(args)>1 then begin
          arc.AddFile(args[0], args[1]) ;
          Writeln('Adding file: '+args[0]+' as '+args[1]) ;
        end
        else begin
        // Добавить вырезание пути к файлу из имени файла для добавления
          arc.AddFile(args[0], args[0]) ;
          Writeln('Adding file: '+args[0]) ;
        end;
      end;
    end;

    script.Free ;

    if zipfile='' then ExitWithError('Not set ZipFile') ;

    SetCompressionLevel(arc, 5);
    SetCompressionMethod(arc, compressionmethod) ;

    if FileExists(zipfile) then DeleteFile(zipfile) ;
    
    arc.SaveToFile(zipfile);
    Writeln('Archive build OK') ;
  except
    on E: Exception do
      Writeln('Ошибка '+E.ClassName+': '+E.Message);
  end;
end ;

end.
