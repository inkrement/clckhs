// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::interfaces(r, cpp)]]
#define RCPP_NEW_DATE_DATETIME_VECTORS 1
#define NA_INTEGER64 LLONG_MIN
#include <Rcpp.h>
#include <clickhouse/client.h>
#include "result.h"
#include <sstream>

using namespace Rcpp;
using namespace clickhouse;

ColumnRef convertDecimalColumn(SEXP v);

// [[Rcpp::export]]
DataFrame fetch(XPtr<Result> res, ssize_t n) {
  return res->fetchFrame(n);
}

// [[Rcpp::export]]
void clearResult(XPtr<Result> res) {
  res.release();
}

// [[Rcpp::export]]
bool hasCompleted(XPtr<Result> res) {
  return res->isComplete();
}

// [[Rcpp::export]]
size_t getRowCount(XPtr<Result> res) {
  return res->numFetchedRows();
}

// [[Rcpp::export]]
size_t getRowsAffected(XPtr<Result> res) {
  return res->numRowsAffected();
}

// [[Rcpp::export]]
std::string getStatement(XPtr<Result> res) {
  return res->getStatement();
}

// [[Rcpp::export]]
std::vector<std::string> resultTypes(XPtr<Result> res) {
  auto colTypes = res->getColTypes();
  std::vector<std::string> r(colTypes.size());
  std::transform(colTypes.begin(), colTypes.end(), r.begin(), [](TypeRef r) { return r->GetName(); });
  return r;
}

// [[Rcpp::export]]
XPtr<Client> connect(String host, int port, String db, String user, String password, String compression) {
  CompressionMethod comprMethod = CompressionMethod::None;
  if(compression == "lz4") {
    comprMethod = CompressionMethod::LZ4;
  } else if(compression != "" && compression != "none") {
    stop("unknown or unsupported compression method '"+std::string(compression)+"'");
  }

  Client *client = new Client(ClientOptions()
            .SetHost(host)
            .SetPort(port)
            .SetDefaultDatabase(db)
            .SetUser(user)
            .SetPassword(password)
            .SetCompressionMethod(comprMethod)
            // (re)throw exceptions, which are then handled automatically by Rcpp
            .SetRethrowException(true));
  XPtr<Client> p(client, true);
  return p;
}

// [[Rcpp::export]]
void disconnect(XPtr<Client> conn) {
  conn.release();
}

// [[Rcpp::export]]
XPtr<Result> select(XPtr<Client> conn, String query) {
  Result *r = new Result(query);
  //TODO: async?
  conn->SelectCancelable(query, [&r] (const Block& block) {
    r->addBlock(block);
    return R_ToplevelExec(checkInterruptFn, NULL) != FALSE;
  });

  XPtr<Result> rp(r, true);
  return rp;
}

// write the contents of an R vector into a Clickhouse column
template<typename CT, typename RT, typename VT>
void toColumn(SEXP v, std::shared_ptr<CT> col, std::shared_ptr<ColumnUInt8> nullCol,
    std::function<VT(typename RT::stored_type)> convertFn) {
  RT cv = Rcpp::as<RT>(v);
  if(nullCol) {
    for(typename RT::stored_type e : cv) {
      bool isNA = RT::is_na(e);
      col->Append(isNA ? 0 : convertFn(e));
      nullCol->Append(isNA);
    }
  } else {
    for(typename RT::stored_type e : cv) {
      if(RT::is_na(e)) {
        stop("cannot write NA into a non-nullable column of type "+
            col->Type()->GetName());
      }
      col->Append(convertFn(e));
    }
  }
}

// write the contents of an R integer64 vector into a Clickhouse column

// Convert to int64_t helper
int64_t* rec(SEXP x){
  int64_t * res = reinterpret_cast<int64_t*>(REAL(x));
  return res;
}

// Convert the vector
std::vector<int64_t> Val(SEXP x){
  if(!Rf_inherits(x, "integer64")){
    warning("Converting to int64_t");
    std::vector<int64_t> retAlt = Rcpp::as<std::vector<int64_t>>(x);
    return retAlt;
  };
  size_t i, n = LENGTH(x);
  std::vector<int64_t> res(n);
  for(i=0; i<n; i++){
    res[i] = rec(x)[i];
  };
  return res;
}

// Special template for integer64 columns to circumvent Rcpp
template<typename CT, typename RT>
void toColumnN(SEXP v, std::shared_ptr<CT> col, std::shared_ptr<ColumnUInt8> nullCol) {
  std::vector<int64_t> cv = Val(v);
  if(nullCol) {
    for(size_t i=0; i<cv.size(); i++) {
      bool isNA = (cv[i] == NA_INTEGER64);
      col->Append(isNA ? 0 : cv[i]);
      nullCol->Append(isNA);
    }
  } else {
    for(size_t i=0; i<cv.size(); i++) {
      if(cv[i] == NA_INTEGER64) {
        stop("cannot write NA into a non-nullable column of type "+
          col->Type()->GetName());
      }
      col->Append(cv[i]);
    }
  }
}


template<typename CT, typename VT>
std::shared_ptr<CT> vecToScalar(SEXP v, std::shared_ptr<ColumnUInt8> nullCol = nullptr) {
  auto col = std::make_shared<CT>();

  int type_of_cor;
  int type_of = TYPEOF(v);

  type_of_cor = Rf_inherits(v, "integer64") ? 99 : type_of;

  switch(type_of_cor) {
    case 99: {
      toColumnN<CT, NumericVector>(v, col, nullCol);
      break;
    }
    case INTSXP: {
      // the lambda could be a default argument of toColumn, but that
      // appears to trigger a bug in GCC
      toColumn<CT, IntegerVector, VT>(v, col, nullCol,
          [](IntegerVector::stored_type x) {return x;});
      break;
    }
    case REALSXP: {
      toColumn<CT, NumericVector, VT>(v, col, nullCol,
          [](NumericVector::stored_type x) {return x;});
      break;
    }
    case LGLSXP: {
      toColumn<CT, LogicalVector, VT>(v, col, nullCol,
          [](LogicalVector::stored_type x) {return x;});
      break;
    }
    case NILSXP:
      // treated as an empty column
      break;
    default:
      stop("cannot write R type "+std::to_string(TYPEOF(v))+
          " to column of type "+col->Type()->GetName());
  }
  return col;
}

template<>
std::shared_ptr<ColumnDate> vecToScalar<ColumnDate, const std::time_t>(SEXP v,
    std::shared_ptr<ColumnUInt8> nullCol) {
  auto col = std::make_shared<ColumnDate>();
  switch(TYPEOF(v)) {
    case REALSXP: {
      toColumn<ColumnDate, DateVector, const std::time_t>(v, col, nullCol,
          Rf_inherits(v, "POSIXct") ?
            [](DateVector::stored_type x) {return x;} :
            [](DateVector::stored_type x) {return x*(60*60*24);});
      break;
    }
    case NILSXP:
      // treated as an empty column
      break;
    default:
      stop("cannot write R type "+std::to_string(TYPEOF(v))+
          " to column of type Date");
  }
  return col;
}

UInt128 parseUUID(const std::string &str) {
  unsigned long long p1, p2, p3, p4, p5;
  int ret = std::sscanf(str.c_str(), "%8llx-%4llx-%4llx-%4llx-%012llx", &p1, &p2, &p3, &p4, &p5);
  if(ret != 5 || str.length() > 36) {
    stop("invalid UUID "+str+"; must be xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx where x are hexadecimal characters");
  }

  uint64_t hi = (p1<<32) | (p2<<16) | p3,
           lo = (p4<<48) | p5;
  return UInt128(hi, lo);
}

template<>
std::shared_ptr<ColumnUUID> vecToScalar<ColumnUUID, UInt128>(SEXP v,
    std::shared_ptr<ColumnUInt8> nullCol) {
  auto col = std::make_shared<ColumnUUID>();
  switch(TYPEOF(v)) {
    case INTSXP:
    case STRSXP: {
      auto sv = Rcpp::as<StringVector>(v);
      if(nullCol) {
        for(auto e : sv) {
          bool isNA = StringVector::is_na(e);
          col->Append(isNA ? ch::UInt128(0, 0) : parseUUID(std::string(e)));
          nullCol->Append(isNA);
        }
      } else {
        for(auto e : sv) {
          if(StringVector::is_na(e)) {
            stop("cannot write NA into a non-nullable column of type "+
                col->Type()->GetName());
          }
          col->Append(parseUUID(std::string(e)));
        }
      }
      break;
    }
    case NILSXP:
      // treated as an empty column
      break;
    default:
      stop("cannot write R type "+std::to_string(TYPEOF(v))+
          " to column of type UUID");
  }
  return col;
}

template<typename CT, typename VT>
std::shared_ptr<CT> vecToString(SEXP v, std::shared_ptr<ColumnUInt8> nullCol = nullptr) {
  auto col = std::make_shared<CT>();
  switch(TYPEOF(v)) {
    case INTSXP:
    case STRSXP: {
      auto sv = Rcpp::as<StringVector>(v);
      if(nullCol) {
        for(auto e : sv) {
          col->Append(std::string(e));
          nullCol->Append(StringVector::is_na(e));
        }
      } else {
        for(auto e : sv) {
          if(StringVector::is_na(e)) {
            stop("cannot write NA into a non-nullable column of type "+
                col->Type()->GetName());
          }
          col->Append(std::string(e));
        }
      }
      break;
    }
    case NILSXP:
      // treated as an empty column
      break;
    default:
      stop("cannot write R type "+std::to_string(TYPEOF(v))+
          " to column of type "+col->Type()->GetName());
  }
  return col;
}

template<typename CT, typename VT>
std::shared_ptr<CT> vecToEnum(SEXP v, TypeRef type, std::shared_ptr<ColumnUInt8> nullCol = nullptr) {
  std::shared_ptr<class EnumType> et = std::static_pointer_cast<EnumType>(type);
  auto iv = Rcpp::as<IntegerVector>(v);
  CharacterVector levels = iv.attr("levels");

  // build a mapping from R factor levels to the enum values in the column type
  // the R levels are contiguous (starting at 1), so a vector works as a "map"
  std::vector<VT> levelMap(levels.size());
  for (size_t i = 0; i < levels.size(); i++) {
    std::string name(levels[i]);
    if (!et->HasEnumName(name)) {
      stop("entry '" + name + "' does not exist in enum type " + et->GetName());
    }
    levelMap[i] = et->GetEnumValue(name);
  }

  auto col = std::make_shared<CT>(type);
  switch(TYPEOF(v)) {
    case INTSXP: {
      toColumn<CT, IntegerVector, VT>(v, col, nullCol,
          [&levelMap](IntegerVector::stored_type x) {
          // subtract 1 since R's factor values start at 1
          return levelMap[x-1];
        });
      break;
    }
    case NILSXP:
      // treated as an empty column
      break;
    default:
      stop("cannot write factor of type " + std::to_string(TYPEOF(v)) +
          " to column of type " + col->Type()->GetName());
  }
  return col;
}

ColumnRef vecToColumn(TypeRef t, SEXP v, std::shared_ptr<ColumnUInt8> nullCol = nullptr) {
  using TC = Type::Code;

  printf("%i insert THIS   !!!\n",t->GetCode());

  switch(t->GetCode()) {
    case TC::Decimal:
      return convertDecimalColumn(v);
    case TC::Int8:
      return vecToScalar<ColumnInt8, int8_t>(v, nullCol);
    case TC::Int16:
      return vecToScalar<ColumnInt16, int16_t>(v, nullCol);
    case TC::Int32:
      return vecToScalar<ColumnInt32, int32_t>(v, nullCol);
    case TC::Int64:
      return vecToScalar<ColumnInt64, int64_t>(v, nullCol);
    case TC::UInt8:
      return vecToScalar<ColumnUInt8, uint8_t>(v, nullCol);
    case TC::UInt16:
      return vecToScalar<ColumnUInt16, uint16_t>(v, nullCol);
    case TC::UInt32:
      return vecToScalar<ColumnUInt32, uint32_t>(v, nullCol);
    case TC::UInt64:
      return vecToScalar<ColumnUInt64, uint64_t>(v, nullCol);
    case TC::UUID:
      return vecToScalar<ColumnUUID, UInt128>(v, nullCol);
    case TC::Float32:
      return vecToScalar<ColumnFloat32, float>(v, nullCol);
    case TC::Float64:
      return vecToScalar<ColumnFloat64, double>(v, nullCol);
    case TC::String:
      return vecToString<ColumnString, const std::string>(v, nullCol);
    case TC::DateTime:
      return vecToScalar<ColumnDateTime, const std::time_t>(v);
    case TC::Date:
      return vecToScalar<ColumnDate, const std::time_t>(v, nullCol);
    case TC::Nullable: {
      // downcast to NullableType to access GetItemType member
      std::shared_ptr<class NullableType> nullable_t = std::static_pointer_cast<NullableType>(t);

      auto nullCtlCol = std::make_shared<ColumnUInt8>();
      auto valCol = vecToColumn(nullable_t->GetNestedType(), v, nullCtlCol);
      return std::make_shared<ColumnNullable>(valCol, nullCtlCol);
    }
    case TC::Array: {
      // downcast to ArrayType to access GetItemType member
      std::shared_ptr<class ArrayType> arr_t = std::static_pointer_cast<ArrayType>(t);

      std::shared_ptr<ColumnArray> arrCol = nullptr;
      Rcpp::List rlist = Rcpp::as<Rcpp::List>(v);

      for(typename Rcpp::List::stored_type e : rlist) {
        auto valCol = vecToColumn(arr_t->GetItemType(), e);
        if (!arrCol) {
          // create a zero-length copy (necessary because the ColumnArray
          // constructor mangles the argument column)
          auto initCol = valCol->Slice(0, 0);
          // initialize the array column with the type of the R vector's first element
          arrCol = std::make_shared<ColumnArray>(initCol);
        }
        arrCol->AppendAsColumn(valCol);
      }
      return arrCol;
    }
    case TC::Enum8:
      return vecToEnum<ColumnEnum8, int8_t>(v, t, nullCol);
    case TC::Enum16:
      return vecToEnum<ColumnEnum16, int16_t>(v, t, nullCol);
    default:
      stop("cannot write unsupported type: "+t->GetName());
  }
}

// [[Rcpp::export]]
void insert(XPtr<Client> conn, String tableName, DataFrame df) {
  StringVector names(df.names());
  std::vector<TypeRef> colTypes;

  // determine actual column types
  conn->Select("SELECT * FROM "+std::string(tableName)+" LIMIT 0", [&colTypes] (const Block& block) {
    if(block.GetColumnCount() > 0 && colTypes.empty()) {
      for(ch::Block::Iterator bi(block); bi.IsValid(); bi.Next()) {
        colTypes.push_back(bi.Type());
      }
    }
  });

  if(colTypes.size() != static_cast<size_t>(df.size())) {
    stop("input has "+std::to_string(df.size())+" columns, but table "+
        std::string(tableName)+" has "+std::to_string(colTypes.size()));
  }

  Block block;
  for(size_t i = 0; i < colTypes.size(); i++) {
    ColumnRef ccol = vecToColumn(colTypes[i], df[i]);
    block.AppendColumn(std::string(names[i]), ccol);
  }
  conn->Insert(tableName, block);
}

ColumnRef convertDecimalColumn(SEXP v) {
// R_CH-column: Rcpp IntegerVector
  IntegerVector cv = Rcpp::as<IntegerVector>(v);


  // CH_CPP-column: sharedPtr to ColumnDecimal with (18,1)
  std::shared_ptr<ColumnDecimal> col  = std::make_shared<ColumnDecimal>(18, 1);


  for(typename IntegerVector::stored_type e : cv) {
    col->Append(e);
  }

  return col;
}



// [[Rcpp::export]]
bool validPtr(SEXP ptr) {
  return R_ExternalPtrAddr(ptr);
}
