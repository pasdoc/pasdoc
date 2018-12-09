unit ok_if_directive;

interface

const
{$if defined(CPUARM) and defined(FPUFPA)}
       MathQNaN : tdoublearray = (0,0,252,255,0,0,0,0);
       MathInf : tdoublearray = (0,0,240,127,0,0,0,0);
       MathNegInf : tdoublearray = (0,0,240,255,0,0,0,0);
       MathPi : tdoublearray =  (251,33,9,64,24,45,68,84);
{$else}
{$ifdef FPC_LITTLE_ENDIAN}
       MathQNaN : tdoublearray = (0,0,0,0,0,0,252,255);
       MathInf : tdoublearray = (0,0,0,0,0,0,240,127);
       MathNegInf : tdoublearray = (0,0,0,0,0,0,240,255);
       MathPi : tdoublearray =  (24,45,68,84,251,33,9,64);
       MathPiExtended : textendedarray = (53,194,104,33,162,218,15,201,0,64);
{$else FPC_LITTLE_ENDIAN}
       MathQNaN : tdoublearray = (255,252,0,0,0,0,0,0);
       MathInf : tdoublearray = (127,240,0,0,0,0,0,0);
       MathNegInf : tdoublearray = (255,240,0,0,0,0,0,0);
       MathPi : tdoublearray =  (64,9,33,251,84,68,45,24);
       MathPiExtended : textendedarray = (64,0,201,15,218,162,33,104,194,53);
{$endif FPC_LITTLE_ENDIAN}
{$endif}

{$if defined(CPUARM) and defined(FPUFPA)}
       Foo1 = 1;
{$else}
       Foo2 = 1;
{$ifdef FPC_LITTLE_ENDIAN}
       Foo3 = 1;
{$else FPC_LITTLE_ENDIAN}
       Foo4 = 1;
{$endif FPC_LITTLE_ENDIAN}
{$ifend}

implementation

end.
