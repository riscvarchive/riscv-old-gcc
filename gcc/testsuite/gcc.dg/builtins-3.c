/* Copyright (C) 2002, 2003  Free Software Foundation.

   Verify that built-in math function constant folding of constant
   arguments is correctly performed by the compiler.

   Written by Roger Sayle, 16th August 2002.  */

/* { dg-do link } */
/* { dg-options "-O2 -ffast-math" } */

extern void link_error(void);

int main()
{
  if (sqrt (0.0) != 0.0)
    link_error ();

  if (sqrt (1.0) != 1.0)
    link_error ();

  if (exp (0.0) != 1.0)
    link_error ();

  if (log (1.0) != 0.0)
    link_error ();

  if (sin (0.0) != 0.0)
    link_error ();

  if (cos (0.0) != 1.0)
    link_error ();

  if (tan (0.0) != 0.0)
    link_error ();

  if (atan (0.0) != 0.0)
    link_error ();


  if (sqrtf (0.0f) != 0.0f)
    link_error ();

  if (sqrtf (1.0f) != 1.0f)
    link_error ();

  if (expf (0.0f) != 1.0f)
    link_error ();

  if (logf (1.0f) != 0.0f)
    link_error ();

  if (sinf (0.0f) != 0.0f)
    link_error ();

  if (cosf (0.0f) != 1.0f)
    link_error ();

  if (tanf (0.0f) != 0.0f)
    link_error ();

  if (atanf (0.0f) != 0.0f)
    link_error ();

  if (sqrtl (0.0l) != 0.0l)
    link_error ();

  if (sqrtl (1.0l) != 1.0l)
    link_error ();

  if (expl (0.0l) != 1.0l)
    link_error ();

  if (logl (1.0l) != 0.0l)
    link_error ();

  if (sinl (0.0l) != 0.0l)
    link_error ();

  if (cosl (0.0l) != 1.0l)
    link_error ();

  if (tanl (0.0l) != 0.0l)
    link_error ();

  if (atanl (0.0) != 0.0l)
    link_error ();

  return 0;
}

