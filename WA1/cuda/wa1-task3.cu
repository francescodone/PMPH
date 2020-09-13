#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <cuda_runtime.h>
#include <sys/time.h>
#include <time.h>
int timeval_subtract( struct timeval* result, struct timeval* t2, struct timeval* t1) {
        unsigned int resolution=1000000;
        long int diff = (t2->tv_usec + resolution * t2->tv_sec) - (t1->tv_usec + resolution * t1->tv_sec) ;
        result->tv_sec = diff / resolution;
        result->tv_usec = diff % resolution;
        return (diff<0);
}
#define GPU_RUNS 100

__global__ void squareKernel(float* d_in, float *d_out, int sizeN) {
        const unsigned int lid = threadIdx.x; // local id inside a block
        const unsigned int gid = blockIdx.x*blockDim.x + lid; // global id
        if(gid<sizeN){
                d_out[gid] = powf((d_in[gid]/(d_in[gid]-2.3)), 3); // do computation
        }
}

int main(int argc, char** argv) {
        unsigned int N = 753411;
        unsigned int mem_size = N*sizeof(float);
        unsigned int block_size = 256;
        unsigned int num_blocks = ((N + (block_size - 1)) / block_size);

        // allocate host memory
        float* h_in = (float*) malloc(mem_size);
        float* h_out = (float*) malloc(mem_size);
        // initialize the memory
        for(unsigned int i=0; i<N; ++i){
                h_in[i] = (float)i;
        }
        // allocate device memory
        float* d_in;
        float* d_out;
        cudaMalloc((void**)&d_in, mem_size);
        cudaMalloc((void**)&d_out, mem_size);
        // copy host memory to device
        cudaMemcpy(d_in, h_in, mem_size, cudaMemcpyHostToDevice);

        unsigned long int elapsed;
        struct timeval t_start, t_end, t_diff;
        gettimeofday(&t_start, NULL);

        // execute the kernel
        for(int i=0; i<GPU_RUNS; i++) {
                squareKernel<<< num_blocks, block_size>>>(d_in, d_out, N);
        }
        cudaThreadSynchronize();


        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_start);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec) / GPU_RUNS;
        printf("Took %d microseconds (%.2fms)nn",elapsed,elapsed/1000.0);


        // copy result from ddevice to host
        cudaMemcpy(h_out, d_out, mem_size, cudaMemcpyDeviceToHost);
        // print result
        //for(unsigned int i=0; i<N; ++i) printf("%.6fnn", h_out[i]);
        // clean-up memory
        free(h_in); free(h_out);
        cudaFree(d_in); cudaFree(d_out);
}