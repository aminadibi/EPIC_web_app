/*
Layout:
1. Basics
2. Settings
3. Radom
4. Input
5. Output
6. Agent
7. Event  
8. Model
*/
 


/* 
Principles:

R variables only need to be updated at Relevant events:
  agent.cumul_exac: ONLY needs to be updated at relevent event (event_exac)  


FEND variables need to be updated at Fixed and End events
  agent.cumul_payoffs are FEND: The background ocmponent (LPT) part is updated on Fixed and END, with one-time costs in Relevant (e.g., exacerbation) but R does NOT update LPT so it is FEND in nature


FREND: they need to be updated at Fixed, Relevant (after exac), and END events; 
  agent.FEV1
  agent.pack_years
  ouput_ex.cumul_time_by_smoking_status technically speaking only needs to be updated when smoking-status changes. But because smoking_status_LPT is FREND (because of pack_years) then we have to deal with this accordingly.
  
END: variables that only need to be updated at end event
  n_alive (number who remained alive) is an END event
  output.cumul_payoff is and END variable simply because we add agent.cumul_payoff to its value at END event(inherently it is FREND)




output_ex.stuff_by_ctime are FEND: They MUST ONLY be updated at fixed and end events.

*/










#define _VERSION_ 20150328

#define OUTPUT_EX 1


#include <Rcpp.h>
using namespace Rcpp; 






enum all_errors
{
  err_incorrect_setting_variable=-1,
  err_incorrect_vector_size=-2,
  err_incorrect_input_var=-3,
  err_event_stack_full=-4,
  err_memory_allocation_failed=-5
} all_errors;


enum record_mode
{
  record_mode_none,
  record_mode_agent,
  record_mode_event
};



enum agent_creation_mode
{
  agent_creation_mode_one,
  agent_creation_mode_all,
  agent_creation_mode_pre
};







/////////////////////////////////////////////////////////////////////BASICS//////////////////////////////////////////////


double calendar_time;



NumericMatrix array_to_Rmatrix(std::vector<double> x, int nCol)
{
  int nRow=x.size()/nCol;
  NumericMatrix y(nRow,nCol);
  //important: CPP is column major order but R is row major; we address the R matrix cell by cell but handle the vector in CPP way;
  for(int i=0;i<nRow;i++)
    for(int j=0;j<nCol;j++)
      y(i,j)=x[i*nCol+j];
  return (y);
}


NumericMatrix array_to_Rmatrix(std::vector<int> x, int nCol)
{
  int nRow=x.size()/nCol;
  NumericMatrix y(nRow,nCol);
  //important: CPP is column major order but R is row major; we address the R matrix cell by cell but handle the vector in CPP way;
  for(int i=0;i<nRow;i++)
    for(int j=0;j<nCol;j++)
      y(i,j)=x[i*nCol+j];
  return (y);
}




  





#define AS_VECTOR_DOUBLE(src) std::vector<double>(&src[0],&src[0]+sizeof(src)/sizeof(double))
#define AS_VECTOR_DOUBLE_SIZE(src,size) std::vector<double>(&src[0],&src[0]+size)

#define AS_MATRIX_DOUBLE(src)  array_to_Rmatrix(std::vector<double>(&src[0][0],&src[0][0]+sizeof(src)/sizeof(double)),sizeof(src[0])/sizeof(double))
#define AS_MATRIX_DOUBLE_SIZE(src)  array_to_Rmatrix(std::vector<double>(&src[0][0],&src[0][0]+size*sizeof(src[0])/sizeof(double)),sizeof(src[0])/sizeof(double))

#define AS_MATRIX_INT(src)  array_to_Rmatrix(std::vector<int>(&src[0][0],&src[0][0]+sizeof(src)/sizeof(int)),sizeof(src[0])/sizeof(int))
#define AS_MATRIX_INT_SIZE(src,size)  array_to_Rmatrix(std::vector<int>(&src[0][0],&src[0][0]+size*sizeof(src[0])/sizeof(int)),sizeof(src[0])/sizeof(int))

#define AS_VECTOR_INT(src) std::vector<int>(&src[0],&src[0]+sizeof(src)/sizeof(int))
#define AS_VECTOR_INT_SIZE(src,size) std::vector<int>(&src[0],&src[0]+size)

#define READ_R_VECTOR(src,dest) {if(src.size()==sizeof(dest)/sizeof(dest[0])) {std::copy(src.begin(),src.end(),&dest[0]); return(0);} else return(err_incorrect_vector_size);}

#define READ_R_MATRIX(src,dest) {if(src.size()==sizeof(dest)/sizeof(dest[0][0])) {std::copy(src.begin(),src.end(),&dest[0][0]); return(0);} else return(err_incorrect_vector_size);}



//////////////////////////////////////////////////////////////////////SETTINGS//////////////////////////////////////////////////

// [[Rcpp::export]]
int Cget_version()
{
  return(_VERSION_);
}


struct settings
{
  int record_mode;    //0: nothing recorded, 1:agents recorded, 2: events recorded;
  
  int agent_creation_mode; //0: one agent at a time; 1: agents are created and saved in agent_stack. 2: saved agents in agent_stack are used (require create_agents before running the model)
  
  int update_continuous_outcomes_mode;    //0:update only on fixed and end events; 1: update before any event;
  
  int runif_buffer_size;
  int rnorm_buffer_size;
  int rexp_buffer_size;
  
  int agent_stack_size;
  int event_stack_size;
};

settings settings{};




// [[Rcpp::export]]
int Cset_settings_var(std::string name,NumericVector value)
{
  if(name=="record_mode") {settings.record_mode=value[0]; return(0);}
  if(name=="agent_creation_mode") {settings.agent_creation_mode=value[0]; return(0);}
  if(name=="update_continuous_outcomes_mode") {settings.update_continuous_outcomes_mode=value[0]; return(0);}
  if(name=="runif_buffer_size") {settings.runif_buffer_size=value[0]; return(0);}
  if(name=="rnorm_buffer_size") {settings.rnorm_buffer_size=value[0]; return(0);}
  if(name=="rexp_buffer_size") {settings.rexp_buffer_size=value[0]; return(0);}
  if(name=="agent_stack_size") {settings.agent_stack_size=value[0]; return(0);}
  if(name=="event_stack_size") {settings.event_stack_size=value[0]; return(0);}
  return(err_incorrect_setting_variable);
}



// [[Rcpp::export]]
List Cget_settings()
{
 return Rcpp::List::create(
      Rcpp::Named("record_mode")=settings.record_mode,
      Rcpp::Named("agent_creation_mode")=settings.agent_creation_mode,
      Rcpp::Named("update_continuous_outcomes_mode")=settings.update_continuous_outcomes_mode,
      Rcpp::Named("runif_buffer_size")=settings.runif_buffer_size,
      Rcpp::Named("rnorm_buffer_size")=settings.rnorm_buffer_size,
      Rcpp::Named("rexp_buffer_size")=settings.rexp_buffer_size,
      Rcpp::Named("agent_stack_size")=settings.agent_stack_size,
      Rcpp::Named("event_stack_size")=settings.event_stack_size
    );
}






struct runtime_stats
{
  int agent_size;
  int n_runif_fills;
  int n_rnorm_fills;
  int n_rexp_fills;
} runtime_stats;


void reset_runtime_stats()
{
  char *x=reinterpret_cast <char *>(&runtime_stats);
  for(unsigned i=0;i<sizeof(runtime_stats);i++)
    x[i]=0;
}


// [[Rcpp::export]]
List Cget_runtime_stats()
{
 return Rcpp::List::create(
      Rcpp::Named("agent_size")=runtime_stats.agent_size,
      Rcpp::Named("n_runif_fills")=runtime_stats.n_runif_fills,
      Rcpp::Named("n_rnorm_fills")=runtime_stats.n_rnorm_fills,
      Rcpp::Named("n_rexp_fills")=runtime_stats.n_rexp_fills
    );
}





////////////////////////////////////////////////////////////////////RANDOM/////////////////////////////////////////////////
//these stuff are internal so no expoert/import;
double *runif_buffer;
long runif_buffer_pointer;

double *rnorm_buffer;
long rnorm_buffer_pointer;

double *rexp_buffer;
long rexp_buffer_pointer;
  
  


double* R_runif(int n)
{
  NumericVector temp(runif(n));
  return(&temp(0));
} 
double* R_runif(int n, double * address)
{
  NumericVector temp(runif(n));
  std::copy(temp.begin(),temp.end(),address);
  return(address);
}
int runif_fill()
{
  R_runif(settings.runif_buffer_size,runif_buffer);
  runif_buffer_pointer=0;
  ++runtime_stats.n_runif_fills;
  return(0);
}
double runif()
{
  if(runif_buffer_pointer==settings.runif_buffer_size) {runif_fill();}
  double temp=runif_buffer[runif_buffer_pointer];
  runif_buffer_pointer++;
  return(temp);
}




double* R_rnorm(int n)
{
  NumericVector temp(rnorm(n));
  return(&temp(0));
}
double* R_rnorm(int n, double * address)
{
  NumericVector temp(rnorm(n));
  std::copy(temp.begin(),temp.end(),address);
  return(address);
}
int rnorm_fill()
{
  R_rnorm(settings.rnorm_buffer_size,rnorm_buffer);
  rnorm_buffer_pointer=0;
  ++runtime_stats.n_rnorm_fills;
  return(0);
}
double rnorm()
{
  if(rnorm_buffer_pointer==settings.rnorm_buffer_size) {rnorm_fill();}
  double temp=rnorm_buffer[rnorm_buffer_pointer];
  rnorm_buffer_pointer++;
  return(temp);
}

//bivariate normal;
void rbvnorm(double rho, double x[2])
{
  x[0]=rnorm();
  double mu=rho*x[0];
  double v=(1-rho*rho);
  
  x[1]=rnorm()*sqrt(v)+mu;
  
}




double* R_rexp(int n)
{
  NumericVector temp(rexp(n,1));
  return(&temp(0));
}
double* R_rexp(int n, double * address)
{
  NumericVector temp(rexp(n,1));
  std::copy(temp.begin(),temp.end(),address);
  return(address);
}
int rexp_fill()
{
  R_rexp(settings.rexp_buffer_size,rexp_buffer);
  rexp_buffer_pointer=0;
  ++runtime_stats.n_rexp_fills;
  return(0);
}
double rexp()
{
  if(rexp_buffer_pointer==settings.rexp_buffer_size) {rexp_fill();}
  double temp=rexp_buffer[rexp_buffer_pointer];
  rexp_buffer_pointer++;
  return(temp);
}


// [[Rcpp::export]]
NumericVector Xrexp(int n, double rate)
{
  double *temp=R_rexp(n);
  return(temp[0]/rate);
}













////////////////////////////////////////////////////////////////////INPUT/////////////////////////////////////////////
struct input
{
  struct
  {
    int time_horizon;
    double discount_cost;
    double discount_qaly;
  } global_parameters;
  
  struct 
  {
    double p_female;
    double age_at_baseline_mu;
    double age_at_baseline_sd;
  } agent;
  
  
  struct
  {
    double inc_by_age_sex[111][2];
    double ces_by_age_sex[111][2];
    double rel_by_age_sex[111][2];
  } smoking;
  
  
  struct
  {
    double pred_fev1_betas_by_sex[4][2];
    double fev1_0_betas[3];
    double fev1_0_res_sd;
    double dfev1_betas[6];  //intercept=-0.05,sex=0,age0=0,fev1_0=0,smoking=-0.02,time=-0.002
    double dfev1_re_sds[2];
    double dfev1_re_rho;
  } lung_function;
  
  
   struct
  {
    double exac_betas[4];     //regression coefficient for logarithm of exacerbation rate (intercept, sex, age, fev1)
    double exac_re_var;       //variance of random-effet (intercept)
    double p_moderate_severe[2]; //probability of moderate or severe, compared with mild, exacerbation
    double exac_end_rate[3]; //rate of exiting exacerbation per type;
    double p_death[3]; //rate of mortality per type;
  } exacerbation;
  
  struct
  {
    double bg_cost_by_stage[4];
    double exac_dcost[3];
  } costs;
  
  struct
  {
    double bg_util_by_stage[4];
    double exac_dutil[3];
  } utilities;
  
  struct 
  {
    double p_by_sex[111][2];
  } bg_death;
  
   
} input;






// [[Rcpp::export]]
List Cget_inputs()
{
 return Rcpp::List::create(
      Rcpp::Named("global_parameters")=Rcpp::List::create(
      Rcpp::Named("time_horizon")=input.global_parameters.time_horizon,
      Rcpp::Named("discount_cost")=input.global_parameters.discount_cost,
      Rcpp::Named("discount_qaly")=input.global_parameters.discount_qaly
      ),
    Rcpp::Named("agent")=Rcpp::List::create(
      Rcpp::Named("p_female")=input.agent.p_female,
      Rcpp::Named("age_at_baseline_mu")=input.agent.age_at_baseline_mu,
      Rcpp::Named("age_at_baseline_sd")=input.agent.age_at_baseline_sd
      ),
    Rcpp::Named("smoking")=Rcpp::List::create(
      Rcpp::Named("inc_by_age_sex")=AS_MATRIX_DOUBLE(input.smoking.inc_by_age_sex),
      Rcpp::Named("ces_by_age_sex")=AS_MATRIX_DOUBLE(input.smoking.ces_by_age_sex),
      Rcpp::Named("rel_by_age_sex")=AS_MATRIX_DOUBLE(input.smoking.rel_by_age_sex)
      ),
    Rcpp::Named("lung_function")=Rcpp::List::create(
      Rcpp::Named("pred_fev1_betas_by_sex")=AS_MATRIX_DOUBLE(input.lung_function.pred_fev1_betas_by_sex),
      Rcpp::Named("fev1_0_betas")=AS_VECTOR_DOUBLE(input.lung_function.fev1_0_betas),
      Rcpp::Named("fev1_0_res_sd")=input.lung_function.fev1_0_res_sd,
      Rcpp::Named("dfev1_betas")=AS_VECTOR_DOUBLE(input.lung_function.dfev1_betas),
      Rcpp::Named("dfev1_re_sds")=AS_VECTOR_DOUBLE(input.lung_function.dfev1_re_sds),
      Rcpp::Named("dfev1_re_rho")=input.lung_function.dfev1_re_rho
      ),
    Rcpp::Named("exacerbation")=Rcpp::List::create(
      Rcpp::Named("exac_betas")=AS_VECTOR_DOUBLE(input.exacerbation.exac_betas),
      Rcpp::Named("exac_re_var")=input.exacerbation.exac_re_var,
      Rcpp::Named("p_moderate_severe")=AS_VECTOR_DOUBLE(input.exacerbation.p_moderate_severe),
      Rcpp::Named("exac_end_rate")=AS_VECTOR_DOUBLE(input.exacerbation.exac_end_rate),
      Rcpp::Named("p_death")=AS_VECTOR_DOUBLE(input.exacerbation.p_death)
      ),
    Rcpp::Named("costs")=Rcpp::List::create(
      Rcpp::Named("bg_cost_by_stage")=AS_VECTOR_DOUBLE(input.costs.bg_cost_by_stage),
      Rcpp::Named("exac_dcost")=AS_VECTOR_DOUBLE(input.costs.exac_dcost)
      ),
    Rcpp::Named("utilities")=Rcpp::List::create(
      Rcpp::Named("bg_util_by_stage")=AS_VECTOR_DOUBLE(input.utilities.bg_util_by_stage),
      Rcpp::Named("exac_dutil")=AS_VECTOR_DOUBLE(input.utilities.exac_dutil)
      ),
    Rcpp::Named("bg_death")=Rcpp::List::create(
      Rcpp::Named("p_by_sex")=AS_MATRIX_DOUBLE(input.bg_death.p_by_sex)
      )
    );
}



// [[Rcpp::export]]
int Cset_input_var(std::string name,NumericVector value)
{
  if(name=="global_parameters$time_horizon") {input.global_parameters.time_horizon=value[0]; return(0);}  
  if(name=="global_parameters$discount_cost") {input.global_parameters.discount_cost=value[0]; return(0);}  
  if(name=="global_parameters$discount_qaly") {input.global_parameters.discount_qaly=value[0]; return(0);}  
  
  if(name=="agent$p_female") {input.agent.p_female=value[0]; return(0);}
  if(name=="agent$age_at_baseline_mu") {input.agent.age_at_baseline_mu=value[0]; return(0);}
  if(name=="agent$age_at_baseline_sd") {input.agent.age_at_baseline_sd=value[0]; return(0);}
  
  if(name=="smoking$inc_by_age_sex") READ_R_MATRIX(value,input.smoking.inc_by_age_sex);
  if(name=="smoking$ces_by_age_sex") READ_R_MATRIX(value,input.smoking.ces_by_age_sex);
  if(name=="smoking$rel_by_age_sex") READ_R_MATRIX(value,input.smoking.rel_by_age_sex);
  
  if(name=="lung_function$pred_fev1_betas_by_sex") READ_R_MATRIX(value,input.lung_function.pred_fev1_betas_by_sex);
  if(name=="lung_function$fev1_0_betas") READ_R_VECTOR(value,input.lung_function.fev1_0_betas);
  if(name=="lung_function$fev1_0_res_sd") {input.lung_function.fev1_0_res_sd=value[0]; return(0);}
  if(name=="lung_function$dfev1_betas") READ_R_VECTOR(value,input.lung_function.dfev1_betas);
  if(name=="lung_function$dfev1_re_sds") READ_R_VECTOR(value,input.lung_function.dfev1_re_sds);
  if(name=="lung_function$dfev1_re_rho") {input.lung_function.dfev1_re_rho=value[0]; return(0);}
  
  if(name=="exacerbation$exac_betas") READ_R_VECTOR(value,input.exacerbation.exac_betas);
  if(name=="exacerbation$exac_re_var") {input.exacerbation.exac_re_var=value[0]; return(0);}
  if(name=="exacerbation$p_moderate_severe") READ_R_VECTOR(value,input.exacerbation.p_moderate_severe);
  if(name=="exacerbation$exac_end_rate") READ_R_VECTOR(value,input.exacerbation.exac_end_rate);
  if(name=="exacerbation$p_death") READ_R_VECTOR(value,input.exacerbation.p_death);
  
  if(name=="costs$bg_cost_by_stage") READ_R_VECTOR(value,input.costs.bg_cost_by_stage);
  if(name=="costs$exac_dcost") READ_R_VECTOR(value,input.costs.exac_dcost);
  if(name=="utilities$bg_util_by_stage") READ_R_VECTOR(value,input.utilities.bg_util_by_stage);
  if(name=="utilities$exac_dutil") READ_R_VECTOR(value,input.utilities.exac_dutil);
  
  if(name=="bg_death$p_by_sex") READ_R_MATRIX(value,input.bg_death.p_by_sex);
  
  return(err_incorrect_input_var);
}









// [[Rcpp::export]]
double xxx(int year, int sex)
{
 return input.bg_death.p_by_sex[year][(int)sex];
}







/////////////////////////////////////////////////////////////////AGENT/////////////////////////////////////






struct agent
{
  long id;
  double local_time;
  bool alive;
  bool sex;
  double dob;
  double age_at_creation;
  
  int smoking_status;   //0:not smoking, positive: current smoking rate (py per year), note that ex smoker status os determined also by pack_years
  double pack_years;
  double smoking_status_LPT;
  
  double fev1; //current fev1, redundant and for speed optimailty;
  double fev1_slope;  //fixed component of rate of decline;
  double fev1_slope_t;  //time-dependent component of FEV1 decline;
  double lung_function_LPT;
  
  double exac_rate;   //backround rate of exacerbation (intercept);
  int cumul_exac[3];    //0:mild, 1:moderate, 2:severe;
  double cumul_exac_time[3];
  double exac_LPT;  //the last time cumul exacerbation time was processed;
  int exac_status;    //current exacerbation status 0: no exacerbation, in 1: mild, 2:moderate, 3:severe exacerbation
  
  double cumul_cost;
  double cumul_qaly;
  double payoffs_LPT;
    
  double tte;
  int event; //carries the last event;
};






agent *agent_stack;
long agent_stack_pointer;

agent smith;


/*
// [[Rcpp::export]]
int Cset_agent_var(long id, std::string name, NumericVector value)    //Imports the agent from 
{
   if(name=="local_time") {agent_stack[id].local_time=value[0]; return(0);}
   if(name=="alive") {agent_stack[id].alive=(value[0]!=0); return(0);}
   if(name=="sex") {agent_stack[id].sex=(value[0]!=0); return(0);}
   if(name=="dob") {agent_stack[id].dob=value[0]; return(0);}
   if(name=="age_at_creation") {agent_stack[id].age_at_creation=value[0]; return(0);}
   
   if(name=="event") {agent_stack[id].event=value[0]; return(0);}
   if(name=="tte") {agent_stack[id].tte=value[0]; return(0);}
    
   return(-1);
}
*/




List get_agent(agent *ag)
{
  return Rcpp::List::create(
      Rcpp::Named("id")=(*ag).id,
      Rcpp::Named("local_time")=(*ag).local_time,
      Rcpp::Named("alive")=(*ag).alive,
      Rcpp::Named("sex")=(int)(*ag).sex,
      Rcpp::Named("dob")=(*ag).dob,
      Rcpp::Named("age_at_creation")=(*ag).age_at_creation,
      
      Rcpp::Named("smoking_status")=(*ag).smoking_status,
      Rcpp::Named("pack_years")=(*ag).pack_years,
            
      Rcpp::Named("fev1")=(*ag).fev1,
      Rcpp::Named("fev1_slope")=(*ag).fev1_slope,
      Rcpp::Named("fev1_slope_t")=(*ag).fev1_slope_t,
      
      Rcpp::Named("exac_status")=(*ag).exac_status,
      Rcpp::Named("exac_rate")=(*ag).exac_rate,
      
      Rcpp::Named("cumul_exac0")=(*ag).cumul_exac[0],
      Rcpp::Named("cumul_exac1")=(*ag).cumul_exac[1],
      Rcpp::Named("cumul_exac2")=(*ag).cumul_exac[2],
      
      Rcpp::Named("cumul_cost")=(*ag).cumul_cost,
      Rcpp::Named("cumul_qaly")=(*ag).cumul_qaly,
                  
      Rcpp::Named("tte")=(*ag).tte,
      Rcpp::Named("event")=(*ag).event
      );
}


//This is a generic function as both agent_stack and event_stack are arrays of agents;
List get_agent(int id, agent agent_pointer[])
{
  return(get_agent(&agent_pointer[id]));
  /*
 return Rcpp::List::create(
      Rcpp::Named("id")=agent_pointer[id].id,
      Rcpp::Named("local_time")=agent_pointer[id].local_time,
      Rcpp::Named("alive")=agent_pointer[id].alive,
      Rcpp::Named("sex")=(int)agent_pointer[id].sex,
      Rcpp::Named("dob")=agent_pointer[id].dob,
      Rcpp::Named("age_at_creation")=agent_pointer[id].age_at_creation,
      
      Rcpp::Named("smoking_status")=agent_pointer[id].smoking_status,
      Rcpp::Named("pack_years")=agent_pointer[id].pack_years,
            
      Rcpp::Named("fev1")=agent_pointer[id].fev1,
      Rcpp::Named("fev1_slope")=agent_pointer[id].fev1_slope,
      Rcpp::Named("fev1_slope_t")=agent_pointer[id].fev1_slope_t,
      
      Rcpp::Named("exac_status")=agent_pointer[id].exac_status,
      
      Rcpp::Named("cumul_exac0")=agent_pointer[id].cumul_exac[0],
      Rcpp::Named("cumul_exac1")=agent_pointer[id].cumul_exac[1],
      Rcpp::Named("cumul_exac2")=agent_pointer[id].cumul_exac[2],
      
      Rcpp::Named("cumul_cost")=agent_pointer[id].cumul_cost,
      Rcpp::Named("cumul_qaly")=agent_pointer[id].cumul_qaly,
                  
      Rcpp::Named("tte")=agent_pointer[id].tte,
      Rcpp::Named("event")=agent_pointer[id].event
      );*/
}



// [[Rcpp::export]]
List Cget_agent(long id)
{
 return(get_agent(id,agent_stack));
}



// [[Rcpp::export]]
List Cget_smith()
{
 return(get_agent(&smith));
}





agent *create_agent(agent *ag,int id)
{
  (*ag).id=id;
  (*ag).alive=1;
  (*ag).local_time=0;
  (*ag).sex=runif()>input.agent.p_female;
  (*ag).age_at_creation=std::min(std::max(rnorm()*input.agent.age_at_baseline_sd+input.agent.age_at_baseline_mu,(double)0),(double)110);
  
  
  
  //smoking
  (*ag).smoking_status=(runif()>0.5);
  (*ag).pack_years=0;
  (*ag).smoking_status_LPT=0;
  
  
  //lung function;
  (*ag).fev1=input.lung_function.fev1_0_betas[0]
            +input.lung_function.fev1_0_betas[1]*(*ag).sex
            +input.lung_function.fev1_0_betas[2]*(*ag).age_at_creation
            +rnorm()*input.lung_function.fev1_0_res_sd;
  (*ag).lung_function_LPT=0;

  
  double temp[2];
  rbvnorm(input.lung_function.dfev1_re_rho,temp);
  (*ag).fev1_slope=input.lung_function.dfev1_betas[0]
                   +temp[0]*input.lung_function.dfev1_re_sds[0]
                   +input.lung_function.dfev1_betas[1]*(*ag).sex
                   +input.lung_function.dfev1_betas[2]*(*ag).age_at_creation
                   +input.lung_function.dfev1_betas[3]*(*ag).fev1
                   +input.lung_function.dfev1_betas[4]*(*ag).smoking_status;

  (*ag).fev1_slope_t=input.lung_function.dfev1_betas[5]
                     +temp[1]*input.lung_function.dfev1_re_sds[1];
                     
                     
                     
  //exacerbation;
  (*ag).exac_rate=exp(
                      input.exacerbation.exac_betas[0]
                      +rnorm()*sqrt(input.exacerbation.exac_re_var)
                      +input.exacerbation.exac_betas[1]*(*ag).sex
                      +input.exacerbation.exac_betas[2]*(*ag).age_at_creation
                      +input.exacerbation.exac_betas[3]*(*ag).fev1
                      );
                      
  (*ag).cumul_exac[0]=0;
  (*ag).cumul_exac[1]=0;
  (*ag).cumul_exac[2]=0;
  (*ag).cumul_exac_time[0]=0;
  (*ag).cumul_exac_time[1]=0;
  (*ag).cumul_exac_time[2]=0;
  (*ag).exac_status=0;
  (*ag).exac_LPT=0;



//payoffs;
  (*ag).cumul_cost=0;
  (*ag).cumul_qaly=0;
  (*ag).payoffs_LPT=0;

  return(ag);
}










// [[Rcpp::export]]
int Ccreate_agents()
{
  if(agent_stack==NULL) return(-1);
  for(int i=0;i<settings.agent_stack_size;i++)
  {
    create_agent(&agent_stack[i],i);
  }
  
  return(0);
}






/////////////////////////////////////////////////////////////////////////OUTPUT/////////////////////////////////////////////////

struct output
{
  int n_agents;
  double cumul_time;    //End variable by nature;
  int n_deaths;         //End variable by nature.
  double total_pack_years;    //END  because agent records
  int total_exac[3];    //0:mild, 1:moderae, 2:severe;    END because agent records
  double total_exac_time[3];  //END because agent records
  double total_cost;    //END because agent records
  double total_qaly;  //END because agent records
} output;




void reset_output()
{
  output.n_agents=0;
  output.cumul_time=0;
  output.n_deaths=0;
  output.total_pack_years=0;
  output.total_exac[0]=0;output.total_exac[1]=0;output.total_exac[2]=0;
  output.total_exac_time[0]=0;output.total_exac_time[1]=0;output.total_exac_time[2]=0;
  output.total_cost=0;
  output.total_qaly=0;
}


// [[Rcpp::export]]
List Cget_outputs()
{
 return Rcpp::List::create(
      Rcpp::Named("n_agents")=output.n_agents,
      Rcpp::Named("cumul_time")=output.cumul_time,
      Rcpp::Named("n_deaths")=output.n_deaths,
      Rcpp::Named("total_exac")=AS_VECTOR_INT(output.total_exac),
      Rcpp::Named("total_exac_time")=AS_VECTOR_DOUBLE(output.total_exac_time),
      Rcpp::Named("total_pack_years")=output.total_pack_years,
      Rcpp::Named("total_cost")=output.total_cost,
      Rcpp::Named("total_qaly")=output.total_qaly
    );
}








#ifdef OUTPUT_EX

struct output_ex
{
  int n_alive_ctime[1000];      //number of folks alive at each fixed time;
  int n_smoking_status_ctime[1000][3];
  int cumul_exac_ctime[1000][3];
  double cumul_cost_ctime[1000];
  double cumul_qaly_ctime[1000];
  double sum_fev1_ltime[1000];
  double cumul_time_by_smoking_status[3];
} output_ex;

#endif



void reset_output_ex()
{
  char *x=reinterpret_cast <char *>(&output_ex);
  for(unsigned i=0;i<sizeof(output_ex);i++)
    x[i]=0;
}


// [[Rcpp::export]]
List Cget_outputs_ex()
{
 return Rcpp::List::create(
      Rcpp::Named("n_alive_ctime")=AS_VECTOR_INT_SIZE(output_ex.n_alive_ctime,input.global_parameters.time_horizon),
      Rcpp::Named("n_smoking_status_ctime")=AS_MATRIX_INT_SIZE(output_ex.n_smoking_status_ctime,input.global_parameters.time_horizon),
      Rcpp::Named("cumul_exac_ctime")=AS_MATRIX_INT_SIZE(output_ex.cumul_exac_ctime,input.global_parameters.time_horizon),
      Rcpp::Named("cumul_cost_ctime")=AS_VECTOR_DOUBLE_SIZE(output_ex.cumul_cost_ctime,input.global_parameters.time_horizon),
      Rcpp::Named("cumul_qaly_ctime")=AS_VECTOR_DOUBLE_SIZE(output_ex.cumul_qaly_ctime,input.global_parameters.time_horizon),
      Rcpp::Named("sum_fev1_ltime")=AS_VECTOR_DOUBLE_SIZE(output_ex.sum_fev1_ltime,input.global_parameters.time_horizon),
      Rcpp::Named("cumul_time_by_smoking_status")=AS_VECTOR_DOUBLE(output_ex.cumul_time_by_smoking_status)
    );
}



//This functionmust run ONLY on start and fixed events; any other place and will mess up!
void update_output_ex(agent *ag)
{
  int time=floor((*ag).local_time+calendar_time);
  int local_time=floor((*ag).local_time);
  
  output_ex.n_alive_ctime[time]+=1;
  
  if((*ag).smoking_status==1)
    output_ex.n_smoking_status_ctime[time][1]+=1;
  else
    if((*ag).pack_years>0)
      output_ex.n_smoking_status_ctime[time][2]+=1;
    else
      output_ex.n_smoking_status_ctime[time][0]+=1;
      
  output_ex.cumul_exac_ctime[time][0]+=(*ag).cumul_exac[0];
  output_ex.cumul_exac_ctime[time][1]+=(*ag).cumul_exac[1];
  output_ex.cumul_exac_ctime[time][2]+=(*ag).cumul_exac[2];
      
  output_ex.cumul_cost_ctime[time]+=(*ag).cumul_cost;
  output_ex.cumul_qaly_ctime[time]+=(*ag).cumul_qaly;
  
  output_ex.sum_fev1_ltime[local_time]+=(*ag).fev1;
}


/*
struct 
{
  int agent_size;
  int n_runif_fill;
  int n_rnorn_fill;
  int n_rexp_fill;
  double elapsed_time;
} runtime_stats;


void reset_runtime_stats()
{
  runtime_stats.agent_size=0;
  runtime_stats.n_runif_fill=0;
  runtime_stats.n_rnorn_fill=0;
  runtime_stats.n_rexp_fill=0;
  runtime_stats.elapsed_time=0;
}
*/





////////////////////////////////////////////////////////////////LPTs/////////////////////////////////



void lung_function_LPT(agent *ag)
{
  double dt=(*ag).local_time-(*ag).lung_function_LPT;
  (*ag).fev1=(*ag).fev1 + (*ag).fev1_slope*dt + 2*(*ag).fev1_slope_t*(*ag).local_time*dt + (*ag).fev1_slope_t*dt*dt;
  (*ag).lung_function_LPT=(*ag).local_time;
}


void smoking_LPT(agent *ag)
{
#ifdef OUTPUT_EX
  if((*ag).smoking_status==0) output_ex.cumul_time_by_smoking_status[0]+=(*ag).local_time-(*ag).smoking_status_LPT;
  else 
    if((*ag).pack_years>0) output_ex.cumul_time_by_smoking_status[2]+=(*ag).local_time-(*ag).smoking_status_LPT;
    else output_ex.cumul_time_by_smoking_status[1]+=(*ag).local_time-(*ag).smoking_status_LPT;
#endif

  (*ag).pack_years+=(*ag).smoking_status*((*ag).local_time-(*ag).smoking_status_LPT);
  (*ag).smoking_status_LPT=(*ag).local_time;
}


void exacerbation_LPT(agent *ag)
{
  if((*ag).exac_status>0)
    (*ag).cumul_exac_time[(*ag).exac_status-1]+=(*ag).local_time-(*ag).exac_LPT;
  (*ag).exac_LPT=(*ag).local_time;
}


void payoffs_LPT(agent *ag)
{
  (*ag).cumul_cost+=input.costs.bg_cost_by_stage[0]*((*ag).local_time-(*ag).payoffs_LPT)/pow(1+input.global_parameters.discount_cost,(*ag).local_time+calendar_time);
  (*ag).cumul_qaly+=input.utilities.bg_util_by_stage[0]*((*ag).local_time-(*ag).payoffs_LPT)/pow(1+input.global_parameters.discount_qaly,(*ag).local_time+calendar_time);
  (*ag).payoffs_LPT=(*ag).local_time;
}







///////////////////////////////////////////////////////////////////EVENT/////////////////////////////////////////////////////////



enum events
{
  event_start=0,
  event_fixed,
  event_smoking_change,
  event_exacerbation,
  event_exacerbation_end,
  event_exacerbation_death,
  event_bg_death,
  event_end
};








agent *event_start_process(agent *ag)
{
#ifdef OUTPUT_EX
  update_output_ex(ag);
#endif
  return(ag);
}


agent *event_end_process(agent *ag)
{
  if((*ag).exac_status>0)
  {
    //NOTE: exacerbation timing is an LPT process and is treated separately. See pdate_continuous_processes
    (*ag).cumul_cost+=input.costs.exac_dcost[(*ag).exac_status-1]/pow(1+input.global_parameters.discount_cost,(*ag).local_time+calendar_time);
    (*ag).cumul_qaly+=input.utilities.exac_dutil[(*ag).exac_status-1]/pow(1+input.global_parameters.discount_qaly,(*ag).local_time+calendar_time);
  }
    
  
  ++output.n_agents;
  output.cumul_time+=(*ag).local_time;
  output.n_deaths+=!(*ag).alive;
  
  lung_function_LPT(ag);
  smoking_LPT(ag);
  exacerbation_LPT(ag);
  payoffs_LPT(ag);
  
  output.total_pack_years+=(*ag).pack_years;
  output.total_exac[0]+=(*ag).cumul_exac[0];
  output.total_exac[1]+=(*ag).cumul_exac[1];
  output.total_exac[2]+=(*ag).cumul_exac[2];
  
  output.total_exac_time[0]+=(*ag).cumul_exac_time[0];
  output.total_exac_time[1]+=(*ag).cumul_exac_time[1];
  output.total_exac_time[2]+=(*ag).cumul_exac_time[2];
  
  output.total_cost+=(*ag).cumul_cost;
  output.total_qaly+=(*ag).cumul_qaly;
  
  
#ifdef OUTPUT_EX
//NO!!! We do not update output_ex here. It might fall within the same calendar year of the last fixed event ad results in double counting. 
//If it falls after that still we ignore as it is a partially observed year.
#endif
  
  return(ag);
}









agent *event_stack;
int event_stack_pointer;






int push_event(agent *ag)
{
  if(event_stack_pointer==settings.event_stack_size) return(err_event_stack_full);
  event_stack[event_stack_pointer]=*ag;
  ++event_stack_pointer;
  return(0);
}




// [[Rcpp::export]]
List Cget_event(int i)
{
 return(get_agent(i,event_stack));
}



// [[Rcpp::export]]
int Cget_n_events() //number of events, noy n events themselves;
{
 return(event_stack_pointer);
}



// [[Rcpp::export]]
DataFrame Cget_agent_events(int id)
{
  DataFrame dfout;
  
  for(int i=0;i<event_stack_pointer;i++)
  {
    if(event_stack[i].id==id)
    {
      dfout.push_back(get_agent(i,event_stack));
    }  
  }
  return(dfout);
}












//////////////////////////////////////////////////////////////////EVENT_SMOKING////////////////////////////////////;
double event_smoking_change_tte(agent *ag)
{
  double rate;
  int age_cut=floor((*ag).age_at_creation+(*ag).local_time);
  if((*ag).smoking_status==1) rate=input.smoking.ces_by_age_sex[age_cut][(*ag).sex];
  else
    if((*ag).pack_years>0) 
      rate=input.smoking.rel_by_age_sex[age_cut][(*ag).sex];
    else
      rate=input.smoking.inc_by_age_sex[age_cut][(*ag).sex];
      
  double tte=rexp()/rate;
  
  return(tte);
}





agent *event_smoking_change_process(agent *ag)
{
  smoking_LPT(ag);
  
  if((*ag).smoking_status==0)
    (*ag).smoking_status=1;
  else
    (*ag).smoking_status=0;
 
   return(ag);
}













//////////////////////////////////////////////////////////////////EVENT_EXACERBATIN////////////////////////////////////;
double event_exacerbation_tte(agent *ag)
{
  if((*ag).exac_status>0) return(HUGE_VAL);
  
  double rate=(*ag).exac_rate;
  
  double tte;
    
  if(rate==0) tte=HUGE_VAL; else tte=rexp()/rate;
  
  return(tte);
}





agent *event_exacerbation_process(agent *ag)
{
  double r=runif();
  
  #define A (1-input.exacerbation.p_moderate_severe[0]-input.exacerbation.p_moderate_severe[1])
  #define B input.exacerbation.p_moderate_severe[0]
  #define C input.exacerbation.p_moderate_severe[1]
  
  if(r<A)
    (*ag).exac_status=1;
  else
    if(r-A<B)
      (*ag).exac_status=2;
    else
      (*ag).exac_status=3;
  
  (*ag).cumul_exac[(*ag).exac_status-1]+=1;
  (*ag).exac_LPT=(*ag).local_time;
  
  return(ag);
}





//////////////////////////////////////////////////////////////////EVENT_EXACERBATIN_END////////////////////////////////////;
double event_exacerbation_end_tte(agent *ag)
{
  if((*ag).exac_status==0) return(HUGE_VAL);
  
  double rate=input.exacerbation.exac_end_rate[(*ag).exac_status-1];
  
  double tte;
    
  if(rate==0) tte=HUGE_VAL; else tte=rexp()/rate;
  
  return(tte);
}





agent *event_exacerbation_end_process(agent *ag)
{
  (*ag).cumul_cost+=input.costs.exac_dcost[(*ag).exac_status-1];
  (*ag).cumul_qaly+=input.utilities.exac_dutil[(*ag).exac_status-1];
  
  (*ag).exac_status=0;
  
  return(ag);
}





//////////////////////////////////////////////////////////////////EVENT_EXACERBATIN_DEATH////////////////////////////////////;
double event_exacerbation_death_tte(agent *ag)
{
  if((*ag).exac_status==0) return(HUGE_VAL);
  
  double tte=HUGE_VAL;
  
  double p=input.exacerbation.p_death[(*ag).exac_status-1];
  
  if(runif()<p)
  {
    tte=1/input.exacerbation.exac_end_rate[(*ag).exac_status-1];
    //All death occur at the end of expected time of exacerbation (for now);
    (*ag).local_time+=tte;
    return(0);
  }
  else
  {
    return(HUGE_VAL);
  }
  
}





agent *event_exacerbation_death_process(agent *ag)
{
  (*ag).alive=false;
  return(ag);
}



////////////////////////////////////////////////////////////////////EVENT_BG_DEATH/////////////////////////////////////;
double event_bg_death_tte(agent *ag)
{
  double age=(*ag).local_time+(*ag).age_at_creation;
  int age_cut=floor(age);
  
  double ttd=HUGE_VAL;
  double p=input.bg_death.p_by_sex[age_cut][(int)(*ag).sex];
  
  //if((*ag).local_time>=5) return(p); else return(1.5);
  
  if(p==1) 
  {
    ttd=0;
  }
  else
  {
    double rate=-log(1-p);
    if(rate>0)  ttd=rexp()/rate; else ttd=HUGE_VAL;
  }
  return(ttd);
}





agent *event_bg_death_process(agent *ag)
{
  (*ag).alive=false;
  return(ag);
}




/////////////////////////////////////////////////////////////////////EVENT_FIXED/////////////////////////////////;
#define EVENT_FIXED_FREQ 1

double event_fixed_tte(agent *ag) 
{
    return(floor((*ag).local_time*EVENT_FIXED_FREQ)/EVENT_FIXED_FREQ+1/EVENT_FIXED_FREQ-(*ag).local_time);
}



agent *event_fixed_process(agent *ag)
{
  lung_function_LPT(ag);
  smoking_LPT(ag);
  exacerbation_LPT(ag);
  payoffs_LPT(ag);
  #ifdef OUTPUT_EX
    update_output_ex(ag);
  #endif
  return(ag);
}













/////////////////////////////////////////////////////////////////////////MODEL///////////////////////////////////////////

// [[Rcpp::export]]
int Callocate_resources()
{
  if(runif_buffer==NULL)
    runif_buffer=(double *)malloc(settings.runif_buffer_size*sizeof(double));
  else
    realloc(runif_buffer,settings.runif_buffer_size*sizeof(double));
  if(runif_buffer==NULL) return(err_memory_allocation_failed); 
  runif_buffer_pointer=settings.runif_buffer_size; //invoikes fill next time;
  
  if(rnorm_buffer==NULL)
    rnorm_buffer=(double *)malloc(settings.rnorm_buffer_size*sizeof(double));
  else
   realloc(rnorm_buffer,settings.rnorm_buffer_size*sizeof(double));
  if(rnorm_buffer==NULL) return(err_memory_allocation_failed);
  rnorm_buffer_pointer=settings.rnorm_buffer_size;
  
  if(rexp_buffer==NULL)
    rexp_buffer=(double *)malloc(settings.rexp_buffer_size*sizeof(double));
  else
    realloc(rexp_buffer,settings.rexp_buffer_size*sizeof(double));
  if(rexp_buffer==NULL) return(err_memory_allocation_failed);
  rexp_buffer_pointer=settings.rexp_buffer_size;
  
  if(agent_stack==NULL)
    agent_stack=(agent *)malloc(settings.agent_stack_size*sizeof(agent));
  else
    realloc(agent_stack,settings.agent_stack_size*sizeof(agent));
  if(agent_stack==NULL) return(err_memory_allocation_failed);
  agent_stack_pointer=0;
  
  if(event_stack==NULL)
    event_stack=(agent *)malloc(settings.event_stack_size*sizeof(agent));
  else
    realloc(event_stack,settings.event_stack_size*sizeof(agent));
  if(event_stack==NULL) return(err_memory_allocation_failed);

  return(0);
}


// [[Rcpp::export]]
List Cget_pointers()
{
  return(Rcpp::List::create(
      Rcpp::Named("runif_buffer_address")=reinterpret_cast<long &>(runif_buffer),
      Rcpp::Named("rnorm_buffer_address")=reinterpret_cast<long &>(rnorm_buffer),
      Rcpp::Named("rexp_buffer_address")=reinterpret_cast<long &>(rexp_buffer),
      Rcpp::Named("agent_stack")=reinterpret_cast<long &>(agent_stack),
      Rcpp::Named("event_stack")=reinterpret_cast<long &>(event_stack)
    )
  );
}



// [[Rcpp::export]]
int Cdeallocate_resources()
{
  try
  {
    if(runif_buffer!=NULL) {free(runif_buffer); runif_buffer=NULL;}
    if(rnorm_buffer!=NULL) {free(rnorm_buffer); rnorm_buffer=NULL;}
    if(rexp_buffer!=NULL) {free(rexp_buffer); rexp_buffer=NULL;}
    if(agent_stack!=NULL) {free(agent_stack); agent_stack=NULL;}
    if(event_stack!=NULL) {free(event_stack); event_stack=NULL;}
  }catch(const std::exception& e){};
  return(0);
}




// [[Rcpp::export]]
int Cdeallocate_resources2()
{
  try
  {
    delete[] runif_buffer;
    delete[] rnorm_buffer;
    delete[] rexp_buffer;
    delete[] agent_stack;
    delete[] event_stack;
  }catch(const std::exception& e){};
  return(0);
}





int Callocate_resources2()
{
   //runif_buffer=(double *)malloc(runif_buffer_size*sizeof(double));
  runif_buffer=new double[settings.runif_buffer_size];
  if(runif_buffer==NULL) return(err_memory_allocation_failed); 
  runif_buffer_pointer=settings.runif_buffer_size; //invoikes fill next time;
  
  rnorm_buffer=new double[settings.rnorm_buffer_size];
  if(rnorm_buffer==NULL) return(err_memory_allocation_failed);
  rnorm_buffer_pointer=settings.rnorm_buffer_size;
  
  rexp_buffer=new double[settings.rexp_buffer_size];
  if(rexp_buffer==NULL) return(err_memory_allocation_failed);
  rexp_buffer_pointer=settings.rexp_buffer_size;
  
  agent_stack=new agent[settings.agent_stack_size];
  if(agent_stack==NULL) return(err_memory_allocation_failed);
  agent_stack_pointer=0;
  
  event_stack=new agent[settings.event_stack_size];
  if(event_stack==NULL) return(err_memory_allocation_failed);

  return(0);
}


// [[Rcpp::export]]
int Cinit_session()
{
  event_stack_pointer=0;
  
  reset_output();
  reset_output_ex();
  reset_runtime_stats(); runtime_stats.agent_size=sizeof(agent);
  
  calendar_time=0;
  
  return(0);
}




// [[Rcpp::export]]
int Cmodel(int n_agents)
{
  
  if(n_agents<1) return(0);
  
  agent *ag;
  
  for(int i=0;i<n_agents;i++)
  {
    //calendar_time=0; NO! calendar_time is set to zero at init_session. Cmodel should be resumable;
        
      
    switch(settings.agent_creation_mode)
    {
      case agent_creation_mode_one:
        ag=create_agent(&smith,i);
        break;
        
      case agent_creation_mode_all:
        ag=create_agent(&agent_stack[i],i);
        break;
        
      case agent_creation_mode_pre:
        ag=&agent_stack[i];
        break;
        
      default:
        return(-1);
    }
    
    (*ag).tte=0;
    event_start_process(ag);
    (*ag).event=event_start;
    if(settings.record_mode==record_mode_event || settings.record_mode==record_mode_agent)
    {
      int _res=push_event(ag);
      if(_res<0) return(_res);
    }

          
    while(calendar_time+(*ag).local_time<input.global_parameters.time_horizon && (*ag).alive)
    {
      double tte=HUGE_VAL;
      int winner=-1;
      double temp;
      
      temp=event_fixed_tte(ag);
      if(temp<tte)
      {
        tte=temp;
        winner=event_fixed;
      }
      
      temp=event_smoking_change_tte(ag);
      if(temp<tte)
      {
        tte=temp;
        winner=event_smoking_change;
      }
      
      temp=event_exacerbation_tte(ag);
      if(temp<tte)
      {
        tte=temp;
        winner=event_exacerbation;
      }
      
      temp=event_exacerbation_end_tte(ag);
      if(temp<tte)
      {
        tte=temp;
        winner=event_exacerbation_end;
      }
      
       temp=event_exacerbation_death_tte(ag);
      if(temp<tte)
      {
        tte=temp;
        winner=event_exacerbation_death;
      }
      
      temp=event_bg_death_tte(ag);
      if(temp<tte)
      {
        tte=temp;
        winner=event_bg_death;
      }
              
              
              
      if(calendar_time+(*ag).local_time+tte<=input.global_parameters.time_horizon)
      {
        (*ag).tte=tte;
        (*ag).local_time=(*ag).local_time+tte;
        
        if(settings.update_continuous_outcomes_mode==1)
        {
           lung_function_LPT(ag);
           smoking_LPT(ag);
            exacerbation_LPT(ag);
            payoffs_LPT(ag);
        }      
        
        switch(winner)
        {
          case event_fixed:
            event_fixed_process(ag);
            (*ag).event=event_fixed;
            break;
          case event_smoking_change:
            event_smoking_change_process(ag);
            (*ag).event=event_smoking_change;
            break;
          case event_exacerbation:
            event_exacerbation_process(ag);
            (*ag).event=event_exacerbation;
            break;
          case event_exacerbation_end:
            event_exacerbation_end_process(ag);
            (*ag).event=event_exacerbation_end;
            break;
          case event_exacerbation_death:
            event_exacerbation_death_process(ag);
            (*ag).event=event_exacerbation_death;
            break;
          case event_bg_death:
            event_bg_death_process(ag);
            (*ag).event=event_bg_death;
            break;
        }
        if(settings.record_mode==record_mode_event)
        {
          int _res=push_event(ag);
          if(_res<0) return(_res);
        }
      }
      else
      {//past time horizon, set the local time to TH as the next step will be agent end;
        (*ag).tte=input.global_parameters.time_horizon-calendar_time-(*ag).local_time;
        (*ag).local_time=(*ag).local_time+tte;
      }
    }//while
    
    event_end_process(ag);
    (*ag).event=event_end;
    if(settings.record_mode==record_mode_event || settings.record_mode==record_mode_agent)
    {
      int _res=push_event(ag);
      if(_res<0) return(_res);
    }
  }
  return(0);
}
