#define mm 2
#define nn (mm+(1<<mm) )
#define outs 1
#define interval 1000
#define worksize (1<<20)  \
 
#define topvar 1
#define botvar nn
#define nnn (botvar+1-topvar)  \
 
#define o mems++
#define oo mems+= 2
#define ooo mems+= 3
#define oooo mems+= 4 \
 
#define pack(lo,hi,dp) (((((octa) (dp) <<20) +(lo) ) <<20) +(hi) )
#define lofield(x) (((x) >>20) &0xfffff)
#define hifield(x) ((x) &0xfffff)
#define depfield(x) (((x) >>40) <<1)
#define extrabit(k,j) (k==0?lofield(node[0][j]) !=hifield(node[0][j]) :0)  \
 
#define makework(j,lo,hi) work[j].l= hi,work[j].r= head[lo].r,head[lo].r= j+1 \
 
/*1:*/
#line 40 "bdd12.w"

#include <stdio.h>
#include <stdlib.h>
/*2:*/
#line 62 "bdd12.w"

typedef unsigned long long octa;

/*:2*//*8:*/
#line 207 "bdd12.w"

typedef struct {
    unsigned int l,r;
} pair;

/*:8*/
#line 43 "bdd12.w"

/*3:*/
#line 99 "bdd12.w"

octa*node[nnn+1];
int qq[nnn+1];

/*:3*//*9:*/
#line 210 "bdd12.w"

pair work[2*worksize],head[worksize];

/*:9*//*13:*/
#line 274 "bdd12.w"

int clone[2*worksize];

/*:13*//*15:*/
#line 317 "bdd12.w"

int map[nnn];
int bitmap[nnn];

/*:15*//*20:*/
#line 428 "bdd12.w"

int b[1<<nnn][nnn];

/*:20*//*24:*/
#line 521 "bdd12.w"

int count1[256],count2[256],count3[256];

/*:24*//*28:*/
#line 577 "bdd12.w"

int cost[1<<nnn];
char routing[1<<nnn];

/*:28*/
#line 44 "bdd12.w"

unsigned long long mems;
/*7:*/
#line 174 "bdd12.w"

void print_level(int k) {
    register int j;
    printf("level %d (x%d):\n",k,map[k]+topvar);
    for(j= 0; j<qq[k]; j++)
        printf(" %d,%d %x\n",
               lofield(node[k][j]),hifield(node[k][j]),
               depfield(node[k][j])+extrabit(k,j));
}

void print_qbdd(void) {
    register int k;
    for(k= topvar-1; k<botvar; k++)print_level(k);
}

/*:7*//*10:*/
#line 216 "bdd12.w"

void print_work(int k) {
    register int lo,hi;
    printf("Current workspace for level %d:\n",k);
    for(lo= 0; lo<qq[k+1]; lo++)
        for(hi= head[lo].r; hi; hi= work[hi-1].r)
            printf(" %d,%d\n",lo,work[hi-1].l);
}

/*:10*//*11:*/
#line 238 "bdd12.w"

void reduce(int k) {
    register int lo,hi,dep,p,nextp,q;
    q= 0;
    for(o,lo= 0; lo<qq[k+1]; lo++) {
        for(o,p= head[lo].r; p; p= nextp) {
            o,nextp= work[p-1].r;
            hi= work[p-1].l;
            if(o,head[hi].l)
                o,clone[p-1]= head[hi].l-1;
            else/*12:*/
#line 259 "bdd12.w"

            {
                if(q>=worksize) {
                    fprintf(stderr,"Sorry, level %d of the QDD is too big (worksize=%d)!\n",
                            k,worksize);
                    exit(-3);
                }
                if(lo!=hi)
                    oo,dep= depfield(node[k+1][lo]|node[k+1][hi])|(1<<k);
                else o,dep= depfield(node[k+1][lo]);
                o,node[k][q]= pack(lo,hi,dep>>1);
                o,clone[p-1]= q;
                o,head[hi].l= ++q;
            }

            /*:12*/
#line 248 "bdd12.w"
            ;
        }
        for(p= head[lo].r; p; p= nextp) {
            o,nextp= work[p-1].r,hi= work[p-1].l;
            o,head[hi].l= 0;
        }
        o,head[lo].r= 0;
    }
    o,qq[k]= q;
}

/*:11*/
#line 46 "bdd12.w"

main() {
    register int h,i,j,k,l,lo,hi,jj,kk,var,cycle;
    octa x;
    /*4:*/
#line 106 "bdd12.w"

    for(k= 0; k<=nnn; k++) {
        j= worksize,kk= k+topvar-1;
        if(nn-kk<5&&j> 1<<(1<<(nn-kk)))j= 1<<(1<<(nn-kk));
        if(kk<20&&((worksize/outs)>>kk)> 0&&(j> outs*(1<<kk)))j= outs*(1<<kk);
        node[k]= (octa*)malloc(j*sizeof(octa));
        if(!node[k]) {
            fprintf(stderr,"I couldn't allocate %d octabytes for node[%d]!\n",j,k);
            exit(-2);
        }
    }

    /*:4*//*6:*/
#line 152 "bdd12.w"

    for(k= 0; k<mm; k++) {
        for(j= 0; j<1<<k; j++)
            node[k][j]= pack(j+j,j+j+1,0);
        qq[k]= 1<<k;
    }
    for(j= 0; j<1<<mm; j++)
        if(j==0)node[mm][j]= pack(0,1,0);
        else node[mm][j]= pack(j+1,j+1,0);
    qq[mm]= 1<<mm;
    for(k= mm+1; k<=nn; k++) {
        for(j= 0; j<nn+2-k; j++)
            if(j<2)node[k][j]= pack(j,j,0);
            else if(j==2)node[k][j]= pack(0,1,0);
            else node[k][j]= pack(j-1,j-1,0);
        qq[k]= nn+2-k;
    }

    /*14:*/
#line 283 "bdd12.w"

    for(k= nnn-1;; k--) {
        for(o,j= 0; j<qq[k]; j++) {
            o,lo= lofield(node[k][j]);
            ooo,makework(j,lo,hifield(node[k][j]));
        }
        reduce(k);
        if(k==0)break;
        for(o,j= 0; j<qq[k-1]; j++) {
            o,x= node[k-1][j];
            ooo,node[k-1][j]= pack(clone[lofield(x)],clone[hifield(x)],0);
        }
    }

    /*:14*/
#line 170 "bdd12.w"
    ;

    /*:6*//*16:*/
#line 321 "bdd12.w"

    for(j= k= 0; j<nnn; k+= 1<<j,j++)o,map[j]= j,bitmap[j]= k;

    /*:16*//*22:*/
#line 463 "bdd12.w"

    /*26:*/
#line 537 "bdd12.w"

    for(j= 0; j<qq[0]; j++) {
        o,i= depfield(node[0][j])+extrabit(0,j);
        for(k= 0; k<nnn; k++)o,b[0][k]+= (i>>k)&1;
    }

    /*:26*/
#line 464 "bdd12.w"
    ;
    for(k= 1; k<nnn; k++)
        /*23:*/
#line 480 "bdd12.w"

    {
        switch(((nnn-2)>>3)*4+((k-1)>>3)) {
        case 2*4+0:
            for(j= 0; j<256; j++)ooo,count1[j]= count2[j]= count3[j]= 0;
            for(o,j= 0; j<qq[k]; j++) {
                x= node[k][j];
                oo,count1[x>>56]++;
                oo,count2[(x>>48)&0xff]++;
                oo,count3[(x>>40)&0xff]++;
            };
            break;
        case 2*4+1:
            for(j= 0; j<256; j++)oo,count1[j]= count2[j]= 0;
            for(o,j= 0; j<qq[k]; j++) {
                x= node[k][j];
                oo,count1[x>>56]++;
                oo,count2[(x>>48)&0xff]++;
            };
            break;
        case 2*4+2:
            for(j= 0; j<256; j++)o,count1[j]= 0;
            for(o,j= 0; j<qq[k]; j++) {
                x= node[k][j];
                oo,count1[x>>56]++;
            };
            break;
        case 1*4+0:
            for(j= 0; j<256; j++)oo,count2[j]= count3[j]= 0;
            for(o,j= 0; j<qq[k]; j++) {
                x= node[k][j];
                oo,count2[(x>>48)&0xff]++;
                oo,count3[(x>>40)&0xff]++;
            };
            break;
        case 1*4+1:
            for(j= 0; j<256; j++)o,count2[j]= 0;
            for(o,j= 0; j<qq[k]; j++) {
                x= node[k][j];
                oo,count2[(x>>48)&0xff]++;
            };
            break;
        case 0*4+0:
            for(j= 0; j<256; j++)o,count3[j]= 0;
            for(o,j= 0; j<qq[k]; j++) {
                x= node[k][j];
                oo,count3[(x>>40)&0xff]++;
            };
            break;
        }
        for(j= k; j<nnn; j++) /*25:*/
#line 524 "bdd12.w"

        {
            l= 1<<((j-1)&0x7),i= 0;
            if(j<9) {
                for(i= 0,h= l; h<256; h= (h+1)|l)o,i+= count3[h];
            } else if(j<17) {
                for(i= 0,h= l; h<256; h= (h+1)|l)o,i+= count2[h];
            } else for(i= 0,h= l; h<256; h= (h+1)|l)o,i+= count1[h];
            oooo,b[bitmap[k]][map[j]]= i;
        }

        /*:25*/
#line 518 "bdd12.w"
        ;
    }

    /*:23*/
#line 466 "bdd12.w"
    ;

    /*:22*/
#line 50 "bdd12.w"
    ;
    for(cycle= 1; cycle<1<<(nnn-1); cycle++) {
        if(cycle%interval==0) {
            printf("Beginning cycle %d (%llu mems so far)\n",cycle,mems);
            fflush(stdout);
        }
        /*21:*/
#line 454 "bdd12.w"

        for(jj= 0,kk= 1,k= cycle; (k&1)==0; kk++)k>>= 1;
        for(k&= k-1; k; jj++,kk++)k&= k-1;
        /*17:*/
#line 337 "bdd12.w"

        o,var= map[kk];
        for(k= kk; k> jj; k--) {
            /*18:*/
#line 349 "bdd12.w"

            for(o,j= 0; j<qq[k-1]; j++) {
                o,x= node[k-1][j],l= lofield(x),h= hifield(x);
                if(k==kk) {
                    oo,lo= lofield(node[k][l]),hi= lofield(node[k][h]);
                    ooo,makework(j+j,lo,hi);
                    lo= hifield(node[k][l]),hi= hifield(node[k][h]);
                    ooo,makework(j+j+1,lo,hi);
                } else {
                    oo,lo= clone[l+l],hi= clone[h+h];
                    ooo,makework(j+j,lo,hi);
                    oo,lo= clone[l+l+1],hi= clone[h+h+1];
                    ooo,makework(j+j+1,lo,hi);
                }
            }

            /*:18*/
#line 340 "bdd12.w"
            ;
            reduce(k);
            oo,map[k]= map[k-1];
            oo,bitmap[k]= bitmap[k-1]|(1<<var);
        }
        /*19:*/
#line 368 "bdd12.w"

        for(o,j= 0; j<qq[jj]; j++) {
            oo,lo= clone[j+j],hi= clone[j+j+1];
            ooo,makework(j,lo,hi);
        }
        reduce(jj);
        o,map[jj]= var;
        if(jj)for(o,j= 0; j<qq[jj-1]; j++) {
                o,x= node[jj-1][j];
                ooo,node[jj-1][j]= pack(clone[lofield(x)],clone[hifield(x)],0);
            }

        /*:19*/
#line 345 "bdd12.w"
        ;

        /*:17*/
#line 457 "bdd12.w"
        ;
        for(k= jj+1; k<=kk; k++)
            /*23:*/
#line 480 "bdd12.w"

        {
            switch(((nnn-2)>>3)*4+((k-1)>>3)) {
            case 2*4+0:
                for(j= 0; j<256; j++)ooo,count1[j]= count2[j]= count3[j]= 0;
                for(o,j= 0; j<qq[k]; j++) {
                    x= node[k][j];
                    oo,count1[x>>56]++;
                    oo,count2[(x>>48)&0xff]++;
                    oo,count3[(x>>40)&0xff]++;
                };
                break;
            case 2*4+1:
                for(j= 0; j<256; j++)oo,count1[j]= count2[j]= 0;
                for(o,j= 0; j<qq[k]; j++) {
                    x= node[k][j];
                    oo,count1[x>>56]++;
                    oo,count2[(x>>48)&0xff]++;
                };
                break;
            case 2*4+2:
                for(j= 0; j<256; j++)o,count1[j]= 0;
                for(o,j= 0; j<qq[k]; j++) {
                    x= node[k][j];
                    oo,count1[x>>56]++;
                };
                break;
            case 1*4+0:
                for(j= 0; j<256; j++)oo,count2[j]= count3[j]= 0;
                for(o,j= 0; j<qq[k]; j++) {
                    x= node[k][j];
                    oo,count2[(x>>48)&0xff]++;
                    oo,count3[(x>>40)&0xff]++;
                };
                break;
            case 1*4+1:
                for(j= 0; j<256; j++)o,count2[j]= 0;
                for(o,j= 0; j<qq[k]; j++) {
                    x= node[k][j];
                    oo,count2[(x>>48)&0xff]++;
                };
                break;
            case 0*4+0:
                for(j= 0; j<256; j++)o,count3[j]= 0;
                for(o,j= 0; j<qq[k]; j++) {
                    x= node[k][j];
                    oo,count3[(x>>40)&0xff]++;
                };
                break;
            }
            for(j= k; j<nnn; j++) /*25:*/
#line 524 "bdd12.w"

            {
                l= 1<<((j-1)&0x7),i= 0;
                if(j<9) {
                    for(i= 0,h= l; h<256; h= (h+1)|l)o,i+= count3[h];
                } else if(j<17) {
                    for(i= 0,h= l; h<256; h= (h+1)|l)o,i+= count2[h];
                } else for(i= 0,h= l; h<256; h= (h+1)|l)o,i+= count1[h];
                oooo,b[bitmap[k]][map[j]]= i;
            }

            /*:25*/
#line 518 "bdd12.w"
            ;
        }

        /*:23*/
#line 459 "bdd12.w"
        ;

        /*:21*/
#line 56 "bdd12.w"
        ;
    }
    /*27:*/
#line 549 "bdd12.w"

    for(k= 1; k<1<<nnn; k++) {
        h= 1<<nnn;
        for(j= 0,i= 1; j<nnn; j++,i<<= 1)if(k&i) {
                oo,l= cost[k^i]+b[k^i][j];
                if(l<h)h= l,lo= j;
            }
        oo,cost[k]= h,routing[k]= lo;
    }
    printf("Optimum ordering (cost %d+externals) can be achieved thus:\n",
           cost[(1<<nnn)-1]);
    for(j= nnn-1,k= (1<<nnn)-1; k; j--,k^= 1<<routing[k])
        printf(" level %d, x%d (%d)\n",
               j,routing[k]+topvar,b[k^(1<<routing[k])][routing[k]]);
    for(k= 1; k<1<<nnn; k++) {
        h= 0;
        for(j= 0,i= 1; j<nnn; j++,i<<= 1)if(k&i) {
                l= cost[k^i]+b[k^i][j];
                if(l> h)h= l,lo= j;
            }
        cost[k]= h,routing[k]= lo;
    }
    printf("Pessimum ordering (cost %d+externals) can be achieved thus:\n",
           cost[(1<<nnn)-1]);
    for(j= nnn-1,k= (1<<nnn)-1; k; j--,k^= 1<<routing[k])
        printf(" level %d, x%d (%d)\n",
               j,routing[k]+topvar,b[k^(1<<routing[k])][routing[k]]);

    /*:27*/
#line 58 "bdd12.w"
    ;
    printf("Altogether %llu mems.\n",mems);
}

/*:1*/
