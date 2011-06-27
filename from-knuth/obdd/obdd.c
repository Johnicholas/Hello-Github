/*1:*/
#line 21 "./obdd.w"

#include "gb_graph.h" 
#include "gb_basic.h" 
#define mm 8
#define nn 8
#define interval 100000 \

#define mate u.V \

#define max_nodes (1<<18)  \
 \

#define hash_rand 314159 \
 \

#define clone right \

#define max_final_nodes (max_nodes/2)  \

#define reduced count \

#define opp v.V \


#line 24 "./obdd.w"

/*3:*/
#line 69 "./obdd.w"

Vertex*black[mm*nn*2],*red[mm*nn*2];
int parity[mm*nn*2];
int edges;
long verbose;
int sols,pseudo_sols;

/*:3*//*5:*/
#line 106 "./obdd.w"

int var[max_nodes],left[max_nodes],right[max_nodes];
int curnode;

/*:5*//*12:*/
#line 220 "./obdd.w"

int hinode;

/*:12*//*14:*/
#line 268 "./obdd.w"

int time;
int hash_f[max_nodes],hash_g[max_nodes],hash_l[max_nodes],hash_t[max_nodes];
int head[mm*nn*2];

/*:14*//*20:*/
#line 379 "./obdd.w"

int count[max_final_nodes];

/*:20*//*26:*/
#line 502 "./obdd.w"

int outerstack[mm*nn*2],innerstack[mm*nn*2];
int outerptr,innerptr;
int total_parity;

/*:26*/
#line 25 "./obdd.w"

/*7:*/
#line 145 "./obdd.w"

void print_obdd()
{
register int k;
for(k= 2;k<curnode;k++)
printf("%d: if %s-%s%s then %d else %d\n",k,
black[var[k]]->name,red[var[k]]->name,parity[var[k]]?"*":"",
right[k],left[k]);
}

/*:7*//*11:*/
#line 206 "./obdd.w"

/*16:*/
#line 285 "./obdd.w"

int new_template(f,g)
register int f,g;
{
register int h;
if(f==0||g==0)return 0;
h= (hash_rand*f+g)&(max_nodes-1);
while(1){
if(hash_t[h]!=time)break;
if(hash_f[h]==f&&hash_g[h]==g)return hash_l[h];
h= (h-1)&(max_nodes-1);
}
hash_t[h]= time;hash_f[h]= f;hash_g[h]= g;hash_l[h]= hinode;
left[hinode]= f;right[hinode]= g;
if(hinode<=curnode){
fprintf(stderr,"Out of memory!\n");exit(-1);
}
return hinode--;
}

/*:16*//*17:*/
#line 310 "./obdd.w"

int new_node(f,g)
register int f,g;
{
register int h;
h= (hash_rand*f+g)&(max_nodes-1);
while(1){
if(hash_t[h]!=time)break;
if(hash_f[h]==f&&hash_g[h]==g)return hash_l[h];
h= (h-1)&(max_nodes-1);
}
hash_t[h]= time;hash_f[h]= f;hash_g[h]= g;hash_l[h]= curnode;
left[curnode]= f;right[curnode]= g;
if(hinode<=curnode){
fprintf(stderr,"Out of memory!\n");exit(-2);
}
return curnode++;
}

/*:17*/
#line 207 "./obdd.w"

int intersect(f,g)
register int f,g;
{
register int j,k;
hinode= max_nodes-1;
/*13:*/
#line 240 "./obdd.w"

{register int source;
/*15:*/
#line 273 "./obdd.w"

time++;
for(k= 0;k<=edges;k++)head[k]= 0;

/*:15*/
#line 242 "./obdd.w"
;
k= new_template(f,g);
source= max_nodes-1;
while(source> hinode){
f= left[source];g= right[source];

j= var[f];k= var[g];
left[source]= new_template(j> k?f:left[f],k> j?g:left[g]);
right[source]= new_template(j> k?f:right[f],k> j?g:right[g]);
if(j> k)j= k;
var[source]= head[j];head[j]= source;
source--;
}
}

/*:13*/
#line 213 "./obdd.w"
;
/*18:*/
#line 345 "./obdd.w"

curnode= 2;
if(head[edges]==0)return 0;
clone[head[edges]]= 1;
for(k= edges-1;k>=0;k--){
time++;
for(j= head[k];j;j= var[j]){
if(clone[left[j]]==clone[right[j]])clone[j]= clone[left[j]];
else{
clone[j]= new_node(clone[left[j]],clone[right[j]]);
var[clone[j]]= k;
}
}
}

/*:18*/
#line 214 "./obdd.w"
;
if(verbose)printf(" ... unreduced size %d, reduced %d\n",
max_nodes-hinode,curnode);
return curnode-1;
}

/*:11*/
#line 26 "./obdd.w"


main()
{
/*4:*/
#line 76 "./obdd.w"

register int j,k,t;
register Vertex*u,*v;
Graph*gg;

/*:4*//*10:*/
#line 187 "./obdd.w"

int f,g;

/*:10*//*25:*/
#line 499 "./obdd.w"

int tt;

/*:25*/
#line 30 "./obdd.w"
;
/*2:*/
#line 47 "./obdd.w"

{
gg= board(mm,nn,0,0,5,0,0);
for(v= gg->vertices,u= gg->vertices+gg->n-1;v<u;v++,u--){
v->mate= u;u->mate= v;
}
k= 0;
for(v= gg->vertices;v<v->mate;v++)if(((v->x.I+v->y.I)&1)==0){
register Arc*a;
for(a= v->arcs;a;a= a->next){
u= a->tip;
black[k]= v;
if(u<u->mate)red[k]= u,parity[k]= 0;
else red[k]= u->mate,parity[k]= 1;
if(verbose)printf("%d: %s--%s%s\n",k,black[k]->name,red[k]->name,
parity[k]?"*":"");
k++;
}
}
edges= k;
}

/*:2*/
#line 31 "./obdd.w"
;
/*8:*/
#line 160 "./obdd.w"

/*6:*/
#line 124 "./obdd.w"

var[0]= var[1]= edges;
left[0]= right[0]= 0;
left[1]= right[1]= 1;
curnode= 2;
for(v= gg->vertices,k= 0;v<v->mate;v++)if(((v->x.I+v->y.I)&1)==0){
j= 0;
while(black[k]==v){
if(j){
var[curnode]= k;left[curnode]= curnode+2;right[curnode]= 0;curnode++;
}
var[curnode]= k;left[curnode]= curnode+2;right[curnode]= curnode+1;
curnode++;
k++;j= 1;
}
left[curnode-1]= 0;
}
left[curnode-2]= right[curnode-1]= 1;

/*:6*/
#line 161 "./obdd.w"
;
f= 2;
for(v= gg->vertices;v<v->mate;v++)if((v->x.I+v->y.I)&1)
/*9:*/
#line 171 "./obdd.w"

{
g= curnode;
for(j= k= 0;k<edges;k++)if(red[k]==v){
if(j){
var[curnode]= k;left[curnode]= curnode+2;right[curnode]= 0;curnode++;
}
var[curnode]= k;left[curnode]= curnode+2;right[curnode]= curnode+1;
curnode++;
j= 1;
}
left[curnode-1]= 0;
left[curnode-2]= right[curnode-1]= 1;
f= intersect(f,g);
}

/*:9*/
#line 164 "./obdd.w"
;

/*:8*/
#line 32 "./obdd.w"
;
/*21:*/
#line 382 "./obdd.w"

if(f>=max_final_nodes){
printf(stderr,"Oops, out of memory for counting!\n");exit(-3);
}
count[0]= 0;count[1]= 1;
for(k= 2;k<=f;k++)count[k]= count[left[k]]+count[right[k]];
printf("Total solutions %d in OBDD of size %d.\n",count[f],f+1);

/*:21*/
#line 33 "./obdd.w"
;
/*24:*/
#line 483 "./obdd.w"

/*23:*/
#line 462 "./obdd.w"

reduced[0]= 0;
for(k= 2,j= 0;k<=f;k++){
left[k]= reduced[left[k]];
if(right[k])reduced[k]= k,right[k]= reduced[right[k]];
else j++,reduced[k]= left[k];
}
printf("(I removed %d null right branches.)\n",j);

/*:23*/
#line 484 "./obdd.w"
;
outerptr= 0;total_parity= parity[0];
tt= right[f];
black[0]->opp= red[0];red[0]->opp= black[0];
traverse:k= var[tt];
black[k]->opp= red[k];red[k]->opp= black[k];
total_parity+= parity[k];
if(right[tt]> 1){
outerstack[outerptr++]= tt;tt= right[tt];goto traverse;
}
/*27:*/
#line 514 "./obdd.w"

t= right[left[f]];
u= black[1]->opp;v= red[1]->opp;u->opp= v;v->opp= u;
in_traverse:k= var[t];
u= black[k]->opp;
if(u==red[k]&&right[t]> 1)goto bypass;
u= black[k]->opp;v= red[k]->opp;u->opp= v;v->opp= u;
total_parity+= parity[k];
if(right[t]> 1){
innerstack[innerptr++]= t;t= right[t];goto in_traverse;
}
/*28:*/
#line 531 "./obdd.w"

if((total_parity&1)==0)
pseudo_sols++;
else{
sols++;
if(sols%interval==0){
printf("%d:",sols);
for(k= 0;k<outerptr;k++)
printf(" %s-%s%s",black[var[outerstack[k]]]->name,
red[var[outerstack[k]]]->name,
parity[var[outerstack[k]]]?"*":"");
for(k= 0;k<innerptr;k++)
printf(" %s-%s%s",black[var[innerstack[k]]]->name,
red[var[innerstack[k]]]->name,
parity[var[innerstack[k]]]?"*":"");
printf("\n");
}
}

/*:28*/
#line 525 "./obdd.w"
;
in_back:k= var[t];total_parity-= parity[k];
u= black[k]->opp;v= red[k]->opp;u->opp= black[k];v->opp= red[k];
bypass:if(left[t]){t= left[t];goto in_traverse;}
if(innerptr){t= innerstack[--innerptr];goto in_back;}

/*:27*/
#line 494 "./obdd.w"
;
back:total_parity-= parity[var[tt]];
if(left[tt]){tt= left[tt];goto traverse;}
if(outerptr){tt= outerstack[--outerptr];goto back;}

/*:24*/
#line 34 "./obdd.w"
;
printf("Total %d solutions and %d pseudo-solutions.\n",sols,pseudo_sols);
}

/*:1*/
