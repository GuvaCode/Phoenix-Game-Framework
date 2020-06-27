
{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

{.$DEFINE EXTENDED_PRECISION}
{$DEFINE DOUBLE_PRECISION}
{.$DEFINE SINGLE_PRECISION}

{$IFNDEF EXTENDED_PRECISION}
   {$IFNDEF DOUBLE_PRECISION}
      {$DEFINE SINGLE_PRECISION}
   {$ENDIF}
{$ELSE}
   {$UNDEF DOUBLE_PRECISION}
{$ENDIF}

// Please unflag to reduce code size if controllers are not needed.
{$DEFINE CONTROLLERS}

// Please unflag to calculate time used by physics in each step.
{$DEFINE COMPUTE_PHYSICS_TIME}

{ Though Delphi 2006(up) supports operator overloading but in fact
  Add(vector1, vector2) performs faster than vector1 + vector2. }
{.$DEFINE BETTER_PERFORMANCE}


{$IFDEF BETTER_PERFORMANCE}
   {$UNDEF OP_OVERLOAD}
{$ENDIF}

{.$UNDEF OP_OVERLOAD}
{$UNDEF EXTENDED_PRECISION}
{$UNDEF DOUBLE_PRECISION}
{$UNDEF ENABLE_DUMP}
{$DEFINE INLINE_AVAIL}

{$DEFINE OP_OVERLOAD}
