#include <Rcpp.h>
#include <string>
#include <sstream>

using namespace Rcpp;

// [[Rcpp::export]]
bool InPolyhedron(Rcpp::NumericVector VertexesX, Rcpp::NumericVector VertexesY, double x, double y) {
  int intersections_num = 0;
  int prev = VertexesX.size() - 1;
  bool prev_under = VertexesY[prev] < y;
  
  for (int i = 0; i < VertexesY.size(); ++i)
  {
    bool cur_under = VertexesY[i] < y;
    
    double ax = VertexesX[prev] - x;
    double ay = VertexesY[prev] - y;
    double bx = VertexesX[i] - x;
    double by = VertexesY[i] - y;
    
    float t = (ax*(by - ay) - ay*(bx - ax));
    if (cur_under && !prev_under)
    {
      if (t > 0)
        intersections_num += 1;
    }
    if (!cur_under && prev_under)
    {
      if (t < 0)
        intersections_num += 1;
    }
    
    prev = i;        
    prev_under = cur_under;        
  }
  
  return (intersections_num&1) != 0;
}


// [[Rcpp::export]]
//Проверяет есть ли в списке переданных параметров все требуемые, или
//содержит ли inList все элементы expList
int ArgIntersection(Rcpp::List inList, Rcpp::List expList) {
  
  int i=0; int len=expList.size(); int err=0;
  //Rcpp::CharacterVector d=expList.names();
  
  for (i=0; i<len; i++){
    if(!inList.containsElementNamed(expList[i])){
      err++;
    };
  };
  return err;
};


// [[Rcpp::export]]
//есть ли в векторе элементы меньше, равные нулю возвращает ИСТИНА если такие есть и ЛОЖЬ, если такие отсутствуют
bool GETZ(Rcpp::NumericVector inVec){
  int i=0; int len=inVec.size();
  for(i=0; i<len; i++){
    if(inVec[i]<0){return false;}
  };
  return true;
};

// [[Rcpp::export]]
// преобразует вектор символов в строку
Rcpp::String CharVecToStr(Rcpp::CharacterVector inVec){
  int i=0; int len=inVec.size(); Rcpp::String s;
  std::string dot = "."; std::string comma = ",";
  for(i=0; i<len-1; i++){
    s+=inVec[i];
    s+=comma;
  };
  s+=inVec[i];
  return s;
};

