#include <stdio.h>
#include <stdlib.h>
#include <math.h>


#define L2UPPER 45
#define L2LOWER 20
#define L3UPPER 25
#define L3LOWER 15
#define L4UPPER 50
#define L4LOWER 20
#define GAMMAUPPER 60
#define GAMMALOWER 50
#define ALPHAUPPER 360
#define ALPHALOWER 0
#define mtrspeed 50
#define TTR 0.5
#define RANGE 50
#define PI 3.14159265

struct chromosome
{
 int L2;
 int L3;
 int L4;
 int gamma;
 int alpha;
};

struct chromosome chromosome[5];

float min_function( float A, float B)
{
 if(A<B)
 {
  return A;
 }
 return B;
};

int random_generator ( int upper, int lower)
{
 int val;
 val=0;
 val= rand() %upper;
 if(val<=lower)
 {
  val=random_generator(upper, lower);
 }
 return val;
};


float fitness_gen( struct chromosome chromosome[], float L1, int i)
//Function for calculating the cumulative error in wing flapping angle and wing flapping frequency
{
 float L5,L6, B, BETA1, BETA2, f1, f2, omega, value, rad, A1, A2,A3,A4, theta1, theta2;
 int M1,M2,cnt;
 rad =PI/180.0f;
 B= 180-(2*chromosome[i].gamma);
 L5=sqrt((L1*L1)+(chromosome[i].L4*chromosome[i].L4)-(2*L1*chromosome[i].L4*cos(rad*chromosome[i].alpha)));
 L6=sqrt((L1*L1)+(chromosome[i].L4*chromosome[i].L4)-(2*L1*chromosome[i].L4*cos(fabs(rad*(chromosome[i].alpha-B)))));
 if((0<=chromosome[i].alpha)<B)
 {
  M1=1;
  M2=1;
 }
 else if(B<=(chromosome[i].alpha)<180)
 {
  M1=1;
  M2=-1;
 }
 else if(180<=(chromosome[i].alpha)<(180+B))
 {
  M1=-1;
  M2=-1;
 }
 else if((180+B)<=(chromosome[i].alpha)<360)
 {
  M1=-1;
  M2=1;
 }
 omega=(PI*mtrspeed)/(30*TTR);
 A1=((chromosome[i].L2*chromosome[i].L2)+(L5*L5)-(chromosome[i].L3*chromosome[i].L3))/(2*chromosome[i].L2*L5);
 A2=((L1*L1)+(L5*L5)-(chromosome[i].L4*chromosome[i].L4))/(2*L1*L5);
 BETA1=acos(A1)+M1*acos(A2);
 A1=((chromosome[i].L2*chromosome[i].L2)+(L6*L6)-(chromosome[i].L3*chromosome[i].L3))/(2*chromosome[i].L2*L6);
 A2=((L1*L1)+(L6*L6)-(chromosome[i].L4*chromosome[i].L4))/(2*L1*L6);
 BETA2=acos(A1)+M2*acos(A2);
 A1=((chromosome[i].L3*chromosome[i].L3)+(L6*L6)-(chromosome[i].L2*chromosome[i].L2))/(2*chromosome[i].L3*L6);
 A2=((chromosome[i].L4*chromosome[i].L4)+(L6*L6)-(L1*L1))/(2*chromosome[i].L4*L6);
 A3=((chromosome[i].L3*chromosome[i].L3)+(L5*L5)-(chromosome[i].L2*chromosome[i].L2))/(2*chromosome[i].L3*L5);
 A4=((chromosome[i].L4*chromosome[i].L4)+(L5*L5)-(L1*L1))/(2*chromosome[i].L4*L5);
 f1=(acos(A1)+M2*acos(A2))-(acos(A3)+M1*acos(A4));
 //error in wing flapping angle between the two wings
 f2=((omega/chromosome[i].L4)*(L1*sin(BETA1)/sin(BETA1+(rad*chromosome[i].alpha))-1)-(omega/chromosome[i].L4)*(L1*sin(BETA2)/sin(-BETA2+(rad*(chromosome[i].alpha-B)))+1));
 //error in wing flapping frequency between two wings
 value = fabs(f1)+ fabs(f2);
 return value;
};

int natural_selection( struct chromosome child[], float p[])
//determines the number of chromosomes to be carried onto next generation through adaptive methods(roulette wheel) or if chromosome is exceptionally healthy
{
 float dummy,rn;
 int N,i,cnt,r;
 cnt=0;
 while(cnt==0)
 {
  r=random_generator(50,0);
  rn=r/100.0f;
  printf("Roulette wheel result is %d\n",r);
  N=0;
  for(i=0;i<5;i++)
  {
   if((rn>p[i]))
   {
    printf("chromosome %d is selected as parent\n",i);
    dummy=child[i].L2;
    child[N].L2=dummy;
    dummy=child[i].L3;
    child[N].L3=dummy;
    dummy=child[i].L4;
    child[N].L4=dummy;
    dummy=child[i].gamma;
    child[N].gamma=dummy;
    dummy=child[i].alpha;
    child[N].alpha=dummy;
    N++;
   }
  }
  printf("Number of parents is %d\n", N);
  if(N==5||N==0)
  {
   printf("repeat roulette wheel slection\n");
   r=random_generator(50,0);
   rn=r/100.0f;
   printf("Roulette wheel result is %d\n",r);
   cnt=0;
  }
  cnt=1;
 }
 return N;
};

void crossover_function( struct chromosome parent[], int N, int L1)
//generates new chromosomes for next generation by randomly changing parameters
{
 float check;
 int i,j,k,dummy,r;
 j=0;
 for(i=N;i<5;)
 {
  r=rand()%6;
  if(r==1)
  {
  dummy=parent[j].L2;
  parent[i].L2=dummy;
  }
  else if(r==2)
  {
  dummy=parent[j].L3;
  parent[i].L3=dummy;
  }
  else if(r==3)
  {
  dummy=parent[j].L4;
  parent[i].L4=dummy;
  }
  else if(r==4)
  {
  dummy=parent[j].gamma;
  parent[i].gamma=dummy;
  }
  else
  {
  dummy=parent[j].alpha;
  parent[i].alpha=dummy;
  }
  check=fitness_gen(parent, L1, i);
  if(check=!check)
  {
   continue;
  }
  j++;
  i++;
 }
 printf("Crossover completed successfuly\n");
};

void mutation_function( struct chromosome c[], int L1)
//Randomly mutates
{
 int r,i;
 float rn,check;
 r=0;
 for(i=0;i<5;)
 {
  r=rand()%100;
  rn=r/100.0f;
  if(rn<0.1)
  {
   r=rand()%8;
   if(r==1)
   {
    c[i].L2=c[i].L2+1;
   }
   else if(r==2)
   {
    c[i].L3=c[i].L3+1;
   }
   else if(r==3)
   {
    c[i].L4=c[i].L4+1;
   }
   else if(r==4)
   {
    c[i].L2=c[i].L2-1;
   }
   else if(r==5)
   {
    c[i].L3=c[i].L3-1;
   }
   else if(r==6)
   {
    c[i].L4=c[i].L4-1;
   }
   else if(r==7)
   {
    c[i].gamma=random_generator(60,50);
   }
   else
   {
    c[i].alpha=random_generator(360,0);
   }
   check=fitness_gen(c,L1,i);
   if(check=!check)
   {
    i--;
   }
   printf("chromosome mutated\n");
  }
  i++;
 }
};


int population_generator( struct chromosome chromo[], float *fitness_val, float L1, int num)
{
 int i,cnt=0;
 float sum, rad, theta1, theta2, A1, A2, dummy, fitness_v[5];
 sum=0;
 rad=(180/PI);
 for(i=0;i<num;)
 {
  chromo[i].L2=random_generator(45,20);
  printf("Value of L2 for chromosome %d is %d\n",i,chromo[i].L2);
  chromo[i].L3=random_generator(25,15);
  printf("Value of L3 for chrmosome %d is %d\n",i,chromo[i].L3);
  chromo[i].L4=random_generator(50,20);
  printf("Value of L4 for chrmosome %d is %d\n",i,chromo[i].L4);
  chromo[i].gamma=random_generator(60,50);
  printf("Value of gamma for chrmosome %d is %d\n",i,chromo[i].gamma);
  chromo[i].alpha=random_generator(360,0);
  printf("Value of alpha for chrmosome %d is %d\n",i,chromo[i].alpha);
  if((L1+chromo[i].L2)<=(chromo[i].L3+chromo[i].L4))
  {
   A1=((chromo[i].L2*chromo[i].L2)+(chromo[i].L3*chromo[i].L3)-((chromo[i].L4-L1)*(chromo[i].L4-L1)))/(2*chromo[i].L2*chromo[i].L3);
   A2=((chromo[i].L2*chromo[i].L2)+(chromo[i].L3*chromo[i].L3)-((chromo[i].L4+L1)*(chromo[i].L4+L1)))/(2*chromo[i].L2*chromo[i].L3);
   if(A1>1||A1<-1)
   {
    printf("Unacceptable values\n");
    continue;
   }
   else if(A2>1||A2<-1)
   {
    printf("Unacceptable values\n");
    continue;
   }
   theta1=rad*acos(A1);
   theta2=180-(rad*acos(A2));
   if(40<=min_function(theta1, theta2)<=140)
   {
    fitness_val[i]= fitness_gen(chromo, L1,i);
    sum=sum+fitness_val[i];
    printf("The fitness value for chromosome number %d is %f\n",i,fitness_val[i]);
    printf("do you want to continue?(0/1)\n");
    scanf("%d",&cnt);
    if(cnt==1)
    {
     i++;
    }
    else
    {
     i=5;
    }
   }
   else
   {
    printf("Unacceptable values\n");
   }
  }
  else
  {
   printf("unacceptable values\n");
  }
 }
 printf("The sum of all fitness is %f\n",sum);
 return sum;
};


void main()
{
 float L1, prob[5], fitness_val[5], sum,dummy;
 struct chromosome parent[5], child[5];
 int i,j,k,cnt,N,resp;
 int seed = time(NULL);
 cnt=1;
 srand(seed);
 printf ("Enter crank length in mm\n");
 scanf ("%f", &L1);
 sum=population_generator(chromosome, fitness_val, L1, 5);
 for(j=0;j==0;)
 {
  for(i=0;i<5;i++)
  {
   dummy=fitness_val[i]/sum;
   prob[i]=dummy;
   printf("The crossover probability for chromosome %d is %f\n",i,prob[i]);
  }
  for(i=0;i<5;i++)
  {
   if (fitness_val[i]<0.05)
   {
    printf("The selected L2 is: %d\t",chromosome[i].L2);
    printf("The selected L3 is: %d\t",chromosome[i].L3);
    printf("The selected L4 is: %d\t",chromosome[i].L4);
    printf("The selected gamma is: %d\n",chromosome[i].gamma);
    printf("For chromosome %d\n", i);
    j++;
    printf("Do you want to continue?\t");
    scanf("%d", &resp);
    if(resp==1)
    {
     j=0;
    }
    else
    {
     j=1;
    }
   }
  }
  N=natural_selection(chromosome, prob);
  crossover_function(chromosome, N, L1);
  mutation_function(chromosome,L1);
  sum=0;
  for(i=0;i<5;i++)
  {
   fitness_val[i]= fitness_gen(chromosome, L1,i);
   sum=sum+fitness_val[i];
   printf("The fitness value for chromosome number %d is %f\n",i,fitness_val[i]);
  }
  printf("Generation %d completed\n",cnt);
  cnt++;
 }
}


























































































