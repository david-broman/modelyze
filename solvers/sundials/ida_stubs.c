/*
    Modeling Kernel Language (MKL) evaluator
    Copyright (C) 2010-2011 David Broman

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


/* Sundials includes */
#include <ida/ida.h>
#include <ida/ida_dense.h>
#include <nvector/nvector_serial.h>
#include <sundials/sundials_math.h>

/* Caml bindings */
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>

/* Structure for storing the context of a 
   simulation instance */
typedef struct _ida_instance
{
  long eqs;        /* Number of equations */
  double reltol;   /* Relative error tolerance */
  double abstol;   /* Absolute error tolerance */
  N_Vector yy;     /* Vector of variables */
  N_Vector yp;     /* Vector of differentiated variables */
  N_Vector id;     /* 1.0 indicates a differentiad var, 0.0 an algebraic */
  void* ida;       /* IDA simulation instance */
  realtype time;   /* The current simulation time */
  value* cb_res;   /* Residual callback function */
  value* cb_root;  /* Root finder callback function */
  int roots;      /* Number of root finder functions */
  int rootsfound;  /* 1 if roots found, else 0 */
  int* foundroots; /* Array with found roots. An element is non-zero if root found */
} ida_instance;

enum PARAMS
{
  START_TIME = 0,
  EQUATIONS = 1,
  RELTOL = 2,
  ABSTOL = 3,
  CB_RES_ID = 4,
  ROOTS = 5,
  CB_ROOT_ID = 6
};

int residual(realtype tres, N_Vector yy, N_Vector yp, 
             N_Vector resval, void *user_data);

int rootfinder(realtype t, N_Vector yy, N_Vector yp,
             realtype *gout, void *user_data);

/* Free the roots allocation */
void free_roots(ida_instance* ctx){
  free((void*)(*ctx).foundroots);
}

/* Free the ida instance */
void free_ida(ida_instance* ctx){
  IDAFree(&((*ctx).ida));
}

/* Free  variable vectors */
void free_yy_yp(ida_instance* ctx){
  N_VDestroy_Serial((*ctx).yy);
  N_VDestroy_Serial((*ctx).yp);
  N_VDestroy_Serial((*ctx).id);
}

/* Free the main simulation instance */
void free_si(ida_instance* ctx){
   free((void*)ctx);
}


/* Make a new simulation instance. Returns 0 if there are errors. */
CAMLprim value ida_make_ext_c(value params, value init_y, value init_yder, value init_id)
{ 
  /* Caml inits */
  CAMLparam4(params,init_y,init_yder,init_id);
  CAMLlocal1(res);

  /* Allocate memory for simulation instance structure */
  ida_instance* ctx =  malloc(sizeof(ida_instance));

  /* Save parameter values */
  (*ctx).time = Double_val(Field(params,START_TIME));
  (*ctx).eqs = Int_val(Field(params,EQUATIONS)); 
  (*ctx).reltol = Double_val(Field(params,RELTOL));
  (*ctx).abstol = Double_val(Field(params,ABSTOL));
  (*ctx).cb_res = caml_named_value(String_val(Field(params,CB_RES_ID))); 
  (*ctx).roots = Int_val(Field(params,ROOTS));  
  (*ctx).cb_root = caml_named_value(String_val(Field(params,CB_ROOT_ID))); 

  /* Init ida structures */
  (*ctx).yy = N_VNew_Serial((*ctx).eqs);
  (*ctx).yp = N_VNew_Serial((*ctx).eqs);
  (*ctx).id = N_VNew_Serial((*ctx).eqs);

  /* Copy initial values */
  realtype* yval = NV_DATA_S((*ctx).yy);
  realtype* ypval = NV_DATA_S((*ctx).yp);
  realtype* idval = NV_DATA_S((*ctx).id);
  unsigned int i;
  for(i=0; i<(*ctx).eqs; i++){    
    yval[i] = RCONST(Double_field(init_y,i));
    ypval[i] = RCONST(Double_field(init_yder,i));     
    idval[i] = RCONST(Double_field(init_id,i));     
  }

  /* Create the IDA simulation instance */  
  (*ctx).ida = IDACreate();
  if((*ctx).ida==0){
    free_yy_yp(ctx);
    free_si(ctx); 
    CAMLreturn(caml_copy_nativeint(0));
  }
  
  /* Init the IDA simulation instance */
  int retval = IDAInit((*ctx).ida, residual, (*ctx).time, (*ctx).yy, (*ctx).yp);
  if(retval != IDA_SUCCESS){
    free_ida(ctx);
    free_yy_yp(ctx);
    free_si(ctx);
    CAMLreturn(caml_copy_nativeint(0));
  }

  /* Set tolerances, linear solver, root finder, and user data */ 
  retval = IDASStolerances((*ctx).ida,(*ctx).reltol,(*ctx).abstol);
  int retval1 = IDA_SUCCESS;
  if((*ctx).roots != 0) 
    retval1 = IDARootInit((*ctx).ida, (*ctx).roots, rootfinder);
  int retval2 = IDADense((*ctx).ida, (*ctx).eqs);
  int retval3 = IDASetUserData((*ctx).ida, ctx);
  if(retval != IDA_SUCCESS || retval1 != IDA_SUCCESS || 
     retval2 != IDA_SUCCESS || retval3 != IDA_SUCCESS){
    free_ida(ctx);
    free_yy_yp(ctx);
    free_si(ctx);
    CAMLreturn(caml_copy_nativeint(0));
  }

  /* Correct initial values */
  retval1 = IDASetId((*ctx).ida,(*ctx).id);
  retval2 = IDACalcIC((*ctx).ida,IDA_YA_YDP_INIT,0.0001);
  if(retval1 != IDA_SUCCESS || retval2 != IDA_SUCCESS){
    free_ida(ctx);
    free_yy_yp(ctx);
    free_si(ctx);
    CAMLreturn(caml_copy_nativeint(0));
  } 

  /* Init roots structures */
  (*ctx).rootsfound = 0;
  (*ctx).foundroots = malloc(sizeof(int) * (*ctx).roots);

  /* Return a pointer to the instance */
  int64 tmp = (int64)ctx;
  res = caml_copy_nativeint(tmp);
  CAMLreturn(res);
}

/* Reinitialize */
CAMLprim void ida_reinit_c(value si)
{
  CAMLparam1(si);
  ida_instance* ctx = (ida_instance*)Nativeint_val(si);
  IDAReInit((*ctx).ida,(*ctx).time,(*ctx).yy,(*ctx).yp);
  CAMLreturn0;
}


/* Close and free memory for an instane */
CAMLprim void ida_close_c(value si)
{
  CAMLparam1(si);
  ida_instance* ctx = (ida_instance*)Nativeint_val(si);
  free_roots(ctx);
  free_ida(ctx);
  free_yy_yp(ctx);
  free_si(ctx);
  CAMLreturn0;
}


/* Returns the number of equations */
CAMLprim value ida_equations_c(value si)
{
  CAMLparam1(si);
  CAMLlocal1(res);
  res = Val_int((*((ida_instance*)Nativeint_val(si))).eqs);
  CAMLreturn(res);
}

/* Returns the relative tolerance error */
CAMLprim value ida_reltol_c(value si)
{
  CAMLparam1(si);
  CAMLlocal1(res);
  res = caml_copy_double((*((ida_instance*)Nativeint_val(si))).reltol);
  CAMLreturn(res);
}

/* Returns the absolute tolerance error */
CAMLprim value ida_abstol_c(value si)
{
  CAMLparam1(si);
  CAMLlocal1(res);
  res = caml_copy_double((*((ida_instance*)Nativeint_val(si))).abstol);
  CAMLreturn(res);
}

/* Returns the array of variables */
CAMLprim value ida_y_c(value si)
{
  CAMLparam1(si);
  CAMLlocal1(res);
  ida_instance* ctx = (ida_instance*)Nativeint_val(si);
  long eqs = (*ctx).eqs;
  res = caml_alloc(eqs*2,Double_array_tag);
  long i;
  realtype* yval = NV_DATA_S((*ctx).yy);
  for(i=0; i<eqs; i++){
    Store_double_field(res, i, yval[i]);
  }
  CAMLreturn(res);
}


/* Returns the array of differentiated variables */
CAMLprim value ida_yp_c(value si)
{
  CAMLparam1(si);
  CAMLlocal1(res);
  ida_instance* ctx = (ida_instance*)Nativeint_val(si);
  long eqs = (*ctx).eqs;
  res = caml_alloc(eqs*2,Double_array_tag);
  long i;
  realtype* ypval = NV_DATA_S((*ctx).yp);
  for(i=0; i<eqs; i++){
    Store_double_field(res, i, ypval[i]);
  }
  CAMLreturn(res);
}


/* Simulate a time step */
CAMLprim value ida_step_c(value si, value steptime)
{ 
  /* Caml inits */
  CAMLparam2(si,steptime);
  ida_instance* ctx = (ida_instance*)Nativeint_val(si);
  realtype newtime =  (*ctx).time + Double_val(steptime);
  int retval = IDASolve((*ctx).ida, newtime, &((*ctx).time), 
			(*ctx).yy, (*ctx).yp, IDA_NORMAL);
  (*ctx).rootsfound = 0;
  if(retval == IDA_ROOT_RETURN){
    (*ctx).rootsfound = 1;
    IDAGetRootInfo((*ctx).ida, (*ctx).foundroots); 
    CAMLreturn(caml_copy_double((*ctx).time));
  }
  else if(retval == IDA_SUCCESS){ 
    CAMLreturn(caml_copy_double((*ctx).time));
  }
  CAMLreturn(caml_copy_double(0));
}


/* Returns the current simulation time */
CAMLprim value ida_time_c(value si)
{
  CAMLparam1(si);
  CAMLlocal1(res);
  res = caml_copy_double((*((ida_instance*)Nativeint_val(si))).time);
  CAMLreturn(res);
}


/* Get root finder info */
CAMLprim value ida_roots_c(value si)
{ 
  /* Caml inits */
  CAMLparam1(si);
  CAMLlocal1(res);
  ida_instance* ctx = (ida_instance*)Nativeint_val(si);
  int roots = (*ctx).roots;
  int i;
  if((*ctx).rootsfound != 0){
    res = caml_alloc(roots,0);
    for(i=0; i<(*ctx).roots; i++)
    Store_field(res,i,Val_int(((*ctx).foundroots)[i])); 
    CAMLreturn(res);
  }
  /* No root */
  CAMLreturn(caml_alloc(0,0));
}


/* Set variable vector */
CAMLprim void ida_set_c(value si, value yy, value yp)
{
  CAMLparam3(si,yy,yp);
  ida_instance* ctx = (ida_instance*)Nativeint_val(si);

  /* Copy values */
  realtype* yval = NV_DATA_S((*ctx).yy);
  realtype* ypval = NV_DATA_S((*ctx).yp);
  unsigned int i;
  for(i=0; i<(*ctx).eqs; i++){    
    yval[i] = RCONST(Double_field(yy,i));
    ypval[i] = RCONST(Double_field(yp,i));     
  }
  CAMLreturn0;
}



/* The system residual function. */
int residual(realtype t, N_Vector yy, N_Vector yp, N_Vector rr, void *user_data)
{
  value v_t;
  value v_yy;
  value v_yp;
  long i;
  realtype *yval, *ypval, *rval;
  yval = NV_DATA_S(yy); 
  ypval = NV_DATA_S(yp); 
  rval = NV_DATA_S(rr);
  
  /* Get user data pointers */
  ida_instance* ctx = (ida_instance*)user_data;
  long eqs = (*ctx).eqs;

  /* Init value y_t and vector value v_yy and v_yp */
  v_t = caml_copy_double(t);
  v_yy = caml_alloc(eqs*2,Double_array_tag);
  v_yp = caml_alloc(eqs*2,Double_array_tag);
  for(i=0; i<eqs; i++){
    Store_double_field(v_yy, i, yval[i]);
    Store_double_field(v_yp, i, ypval[i]);
  }
 
  /* Callback to OCaml code */
  value v_rr = caml_callback3(*((*ctx).cb_res), v_t, v_yy, v_yp);
  
  /* Copy back the residual data */
  for(i=0; i<eqs; i++){
    rval[i] = RCONST(Double_field(v_rr,i));
  }
    
  return(0);
}



/* Root function routine. Compute functions g_i(t,y) for i = 0,1. */
int rootfinder(realtype t, N_Vector yy, N_Vector yp, realtype *gout, void *user_data)
{
  value v_t; /* When using callbacks, CAMLlocal() must not be called */
  value v_yy;
  value v_yp;
  long i;
  realtype *yval, *ypval;
  yval = NV_DATA_S(yy); 
  ypval = NV_DATA_S(yp); 

  /* Get user data pointers */
  ida_instance* ctx = (ida_instance*)user_data;
  long eqs = (*ctx).eqs;
  long roots = (*ctx).roots;

  /* Init value y_t and vector value v_yy and v_yp */
  v_t = caml_copy_double(t);
  v_yy = caml_alloc(eqs*2,Double_array_tag);
  v_yp = caml_alloc(eqs*2,Double_array_tag);
  for(i=0; i<eqs; i++){
    Store_double_field(v_yy, i, yval[i]);
    Store_double_field(v_yp, i, ypval[i]);
  }
 
  /* Callback to OCaml code */ 
  value v_g = caml_callback3(*((*ctx).cb_root), v_t, v_yy, v_yp);
   
  /* Copy back the residual data */
  for(i=0; i<roots; i++){
    gout[i] = RCONST(Double_field(v_g,i));
  }

  return(0);
}


