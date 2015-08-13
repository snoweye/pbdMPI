#include "spmd.h"

/* ----- alltoall ----- */
SEXP spmd_alltoall_integer(SEXP R_send_data, SEXP R_send_count, SEXP comm_size, 
		SEXP R_comm){
	SEXP R_recv_data;
	const int send_count = INTEGER(R_send_count)[0];
	const int len = INTEGER(comm_size)[0] * send_count;
	PROTECT(R_recv_data = allocVector(INTSXP, len));
	for (int i=0; i<len; i++)
		INTEGER(R_recv_data)[i] = 0;
	
	spmd_errhandler(MPI_Alltoall(INTEGER(R_send_data),
		send_count,
		MPI_INT, INTEGER(R_recv_data), send_count,
		MPI_INT, comm[INTEGER(R_comm)[0]]));
	
	UNPROTECT(1);
	return(R_recv_data);
} /* End of spmd_alltoall_integer(). */

SEXP spmd_alltoall_double(SEXP R_send_data, SEXP R_send_count, SEXP comm_size, 
		SEXP R_comm){
	SEXP R_recv_data;
	const int send_count = INTEGER(R_send_count)[0];
	const int len = INTEGER(comm_size)[0] * send_count;
	PROTECT(R_recv_data = allocVector(REALSXP, len));
	for (int i=0; i<len; i++)
		REAL(R_recv_data)[i] = 0;
	
	spmd_errhandler(MPI_Alltoall(REAL(R_send_data),
		send_count,
		MPI_DOUBLE, REAL(R_recv_data), send_count,
		MPI_DOUBLE, comm[INTEGER(R_comm)[0]]));
	
	UNPROTECT(1);
	return(R_recv_data);
} /* End of spmd_alltoall_double(). */

SEXP spmd_alltoall_raw(SEXP R_send_data, SEXP R_recv_data,
		SEXP R_send_count, SEXP R_recv_count, SEXP R_comm){
	spmd_errhandler(MPI_Alltoall(RAW(R_send_data),
		INTEGER(R_send_count)[0],
		MPI_BYTE, RAW(R_recv_data), INTEGER(R_recv_count)[0],
		MPI_BYTE, comm[INTEGER(R_comm)[0]]));
	return(R_recv_data);
} /* End of spmd_alltoall_raw(). */

