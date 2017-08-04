/*
Copyright (C) 2017 Bartosz Meglicki <meglickib@gmail.com>

This file is part of dbscan1d-r R package.

dbscan1d-r is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

dbscan1d-r is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with dbscan1d-r.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <Rcpp.h>
using namespace Rcpp;

const int NOT_VISITED=-1, NOISE=0;

void calculate_neighborhood(const NumericVector &X,const double eps, IntegerVector &L, IntegerVector &U);
void find_clusters(const NumericVector &X, const IntegerVector &L, const IntegerVector &U, IntegerVector &C,const int min_points);
int neighborhood_size(const int i, const IntegerVector &L, const IntegerVector &U);
void expand_cluster(const NumericVector &X,const int p, const IntegerVector &L, const IntegerVector &U, IntegerVector &C,const int cluster_id,const int min_points);

//' @title Efficent implementation of DBSCAN alghorithm for 1D data
//' @description Perfoms clustering with DBSCAN alghorithm optimized for one dimensional data.
//' For sorted data (pass FALSE to sort parameter) the complexity is O(n) where n is the length of the data vector.
//' If sorting is needed the algorithm complexity is O(nlogn).
//' 
//' @param data numeric vector with points to cluster
//' @param eps the epsilon-neighborhood value for clustering
//' @param minPoints the number of points for core-point criterion
//' @param sort whether to sort the data, pass FALSE if calling for sorted data
//' @return a vector of cluster labels
//'
//' @examples 
//' dbscan1d(c(9,8,7, 1,2,3, 15,16,17), eps=1, minPoints=2)
//'
//' @export
// [[Rcpp::export]]
IntegerVector dbscan1d(const NumericVector data, double eps, int minPoints, bool sort=true)
{
	const int N=data.size();
	IntegerVector C(N, NOT_VISITED), L(N), U(N);

	NumericVector sorted_data;
	std::vector<size_t> IND;	
	
	if(sort)
	{	//store the order of data in IND value, apply order in auxialiary sorted_data
		sorted_data=NumericVector(N);
		IND.resize(N);

		std::iota(IND.begin(), IND.end(), 0);
		std::sort(IND.begin(), IND.end(),[&data](size_t i1, size_t i2) {return data[i1] < data[i2];});
		
		for(int i=0;i<N;++i)
			sorted_data[i]=data[IND[i]];
	}
	//from now on X points to sorted data
	const NumericVector &X = sort ? sorted_data : data;
	
	calculate_neighborhood(X, eps, L, U);
	
	find_clusters(X, L, U, C, minPoints);
	
	if(!sort)
		return C;
	
	//use L to get back to original order for cluster labels
	for(int i=0;i<N;++i)
		L[IND[i]]=C[i];
		
	return L;
}

void calculate_neighborhood(const NumericVector &X,const double eps, IntegerVector &L, IntegerVector &U)
{
	const int N=X.size();
	int u=0, l=N-1;
	
	for(int i=0;i<N;++i)
	{
		while(u<N && std::abs(X[i]-X[u]) <= eps)
			++u;
		U[i]=u-1;
	}
	
	for(int i=N-1;i>=0;--i)
	{
		while(l>=0 && std::abs(X[i]-X[l]) <= eps)
			--l;
		L[i]=l+1;
	}	
}

void find_clusters(const NumericVector &X, const IntegerVector &L, const IntegerVector &U, IntegerVector &C,const int min_points)
{
	const int N=X.size();
	int cluster_id=0;
	
	for(int i=0;i<N;++i)
	{
		if(C[i] != NOT_VISITED)
			continue;
		if(neighborhood_size(i, L, U) < min_points)
			C[i]=NOISE;
		else
			expand_cluster(X, i, L, U, C, ++cluster_id, min_points);
	}
	
}

int neighborhood_size(const int i, const IntegerVector &L, const IntegerVector &U)
{
	return U[i]-L[i]+1;
}

void expand_cluster(const NumericVector &X,const int p, const IntegerVector &L, const IntegerVector &U, IntegerVector &C,const int cluster_id,const int min_points)
{
	const int N=X.size();
	int u=U[p], l=L[p], i;
	C[p]=cluster_id;
	
	for(i=p+1;i<N && i<=u;++i)
		if(C[i]==NOT_VISITED)
		{
			C[i]=cluster_id;
			if(neighborhood_size(i, L, U)>=min_points)
				u=U[i];
		}
		else if(C[i]==NOISE)
			C[i]=cluster_id;
			
	u=i-1;
	
	for(i=p-1;i>=0 && i>=l;--i)
		if(C[i]==NOT_VISITED)
		{
			C[i]=cluster_id;
			if(neighborhood_size(i, L, U)>=min_points)
				l=L[i];
		}
		else if(C[i]==NOISE)
			C[i]=cluster_id;
	
	l=i+1;
}
