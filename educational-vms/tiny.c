// Author: Riccardo Poli (rpoli@essex.ac.uk)

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAX_INPUTS 10
#define MAX_EXAMPLES 1000
#define MAX_LEN 10000
#define POPSIZE 10000
#define DEPTH   4
#define PMUT_PER_NODE    0.02f
#define CROSSOVER_PROB   0.9f
#define GENERATIONS 50
#define TSIZE 2

enum {ADD=MAX_INPUTS, SUB, MUL, DIV};
char *prim_print = "+-*/";

#define FSET_START ADD
#define FSET_END DIV

#define RANDOM_FUNCTION lrand48() % (FSET_END - FSET_START + 1) + FSET_START;

float *x;
char *program;
short int varnumber, fitnesscases;
int best;
float favgpop;
long seed;
float avg_len; 
unsigned long nodes_evaluated = 0;
float targets[MAX_EXAMPLES][MAX_INPUTS+2];

float run() /* Interpreter */
{
  char primitive = *program++;
  float num, den;
  switch ( primitive )	
    {
    case ADD : return( run() + run() );
    case SUB : return( run() - run() );
    case MUL : return( run() * run() );
    case DIV : 
      num = run();
      den = run();
      return( fabs( den ) <= 0.001f ? num : num / den );
    default:
      return(x[primitive]);
    }
}
	    
char *traverse( char *buffer )
{
  return ( *buffer < FSET_START ? ++buffer : traverse( traverse( ++buffer ) ) );
}

int length( char *buffer )
{
  return ( traverse(buffer) - buffer);
}



float fitness_function( char *Prog ) 
{
  short int i;
  float fit = 0.0f;
  nodes_evaluated += length( Prog )*fitnesscases;
  for (i = 0; i < fitnesscases; i ++ )
    {
      x = targets[i];
      program = Prog;
      fit += fabs( run() - x[varnumber]);
    }
  return(-fit );
}



int grow( char *buffer, int pos, int max, int depth )
{
  char prim = !pos ? 1 : lrand48() % 2;
  if ( pos >= max ) return( -1 );
  if ( !prim  || !depth )
    {
      buffer[pos] = lrand48() % varnumber;
      return(++pos);
    }
  else
    {
      buffer[pos] = RANDOM_FUNCTION;
      return( grow( buffer, grow( buffer, ++pos, max,--depth), max,depth ) );
    }
}

char *print_indiv( char *buffer )
{
  if ( *buffer < FSET_START )
    {
      printf( " X%d", *buffer );
      return( ++buffer );
    }
  printf(" (%c",prim_print[*buffer - FSET_START]);
  buffer = print_indiv( print_indiv( ++buffer ) );
  printf(")");
  return( buffer );
}


char **create_random_pop(int n, int depth, float *fitness )
{
  char **pop = (char **) malloc( n * sizeof(char *));
  char buffer[MAX_LEN];
  int i, len;
  for ( i = 0; i < n; i ++ )
    {
      do  len = grow( buffer, 0, MAX_LEN, depth ); while (len < 0 );
      pop[i] = memcpy( (char *) malloc( len ), buffer, len );
      fitness[i] = fitness_function( pop[i] );
    }
  return( pop );
}

void stats( float *fitness, char **pop, int gen )
{
  int i, node_count;
  node_count = best = 0;
  favgpop = 0.0f;
  for ( i = 0; i < POPSIZE; i ++ )
    {
      node_count +=  length( pop[i] );
      favgpop += fitness[i];
      if ( fitness[i] > fitness[best] ) 
	best = i;
    }
  avg_len = (float) node_count / POPSIZE;
  favgpop /= POPSIZE;
  printf("\nGen=%d AvgFit=%g BestFit=%g Nodes=%lu Avg_size=%g\n",
         gen, -favgpop,  -fitness[best],nodes_evaluated,avg_len);
  print_indiv( pop[best] );
}

int tournament( float *fitness, int tsize, char type )
{
  int bestworst,  competitor;
  char i;
  float  fbestworst = type ? -1e30f : 1e30f;
  for ( i = 0; i < tsize; i ++ )
    {
      competitor = lrand48() % POPSIZE;
      if ( type ? fitness[competitor] > fbestworst : fitness[competitor] < fbestworst )
	{
	  fbestworst = fitness[competitor];
	  bestworst = competitor;
	}
    }
  return( bestworst );
}


char *crossover( char *parent1, char *parent2 )
{
  char *xo1start, *xo1end, *xo2start, *xo2end, *offspring;
  int len1 = length( parent1 );
  int len2 = length( parent2 );
  xo1start =  parent1 + lrand48() % len1;
  xo1end = traverse( xo1start );
  xo2start =  parent2 + lrand48() % len2;
  xo2end = traverse( xo2start );
  offspring = malloc( ((xo1start - parent1) + (xo2end - xo2start) + (parent1+len1-xo1end)) );
  memcpy( offspring, parent1, (xo1start - parent1) );
  memcpy( offspring + (xo1start - parent1), xo2start,  (xo2end - xo2start) );
  memcpy( offspring + (xo1start - parent1) + (xo2end - xo2start), xo1end, 
	  (parent1+len1-xo1end) );
  return( offspring );
}


char *mutation( char *parent, double pmut )
{
  int len = length( parent ), i;
  char *mutsite;
  char *parentcopy =  memcpy( (char *) malloc( len ), parent, len );
  for (i = 0; i < len; i ++ )
    if ( drand48() < pmut )
      {
	mutsite =  parentcopy + i;
	*mutsite =  *mutsite < FSET_START ? lrand48() % varnumber :
	  RANDOM_FUNCTION;
      }
  return( parentcopy );
}

int main()  
{
  int gen, indivs, offspring, parent1,  i, j;
  float *fitness = (float *) calloc( POPSIZE, sizeof(float));
  char **pop;
  scanf( "%ld%hd%hd", &seed, &varnumber, &fitnesscases);
  seed = seed ? (long) time( NULL ): seed;
  srand48(seed);
  if (varnumber > MAX_INPUTS || fitnesscases >= MAX_EXAMPLES ) 
    perror("var/example #");
  for (i = 0; i < fitnesscases; i ++ )
    for (j = 0; j <= varnumber; j++)
      scanf("%g",&(targets[i][j]));
  pop = create_random_pop(POPSIZE, DEPTH, fitness );
  printf("SEED=%ld\nMAXLEN=%d\nPSIZE=%d\nDEPTH=%d\nXOPROB=%g\nPMUT=%g\nGENS=%d\nTSIZE=%d",
 seed, MAX_LEN, POPSIZE, DEPTH, CROSSOVER_PROB, PMUT_PER_NODE, GENERATIONS, TSIZE );
  stats( fitness, pop, 0 );
  for ( gen = 1; gen < GENERATIONS; gen ++ )
    {
      if (  fitness[best] > -1e-5f ) 
	{
	  printf("\nOK\n");
	  exit( 0 );
	}
      for ( indivs = 0; indivs < POPSIZE; indivs ++ )
	{
	  parent1 = tournament( fitness, TSIZE,1 );
	  offspring = tournament( fitness, TSIZE,0 );
	  free( pop[offspring] );
	  pop[offspring] = drand48() > CROSSOVER_PROB ?
	    crossover( pop[parent1],pop[tournament( fitness, TSIZE,1 )] ) :
	    mutation( pop[parent1], PMUT_PER_NODE );
	  fitness[offspring] = fitness_function( pop[offspring] );
	}
      stats( fitness, pop, gen );
    }
  printf("\nNO\n");
  exit(1);
}

