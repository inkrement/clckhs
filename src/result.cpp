#include <stdexcept>
#include "result.h"


// helper function which emits an R warning without causing a longjmp
// see https://stackoverflow.com/questions/24557711/how-to-generate-an-r-warning-safely-in-rcpp
void warn(std::string text) {
      Rcpp::Function warning("warning");
      warning(text);
}

Result::Result(std::string stmt) {
  statement = stmt;
}

template<typename CT, typename RT>
void Result::convertTypedColumn(AccFunc colAcc, Rcpp::List &df,
    size_t start, size_t len,
    ConvertFunc<CT, RT> convFunc) const {
  RT v(len);   // R vector for the column

  size_t i = 0, offset = 0;
  for(const ColBlock &cb : columnBlocks) {
    const ch::ColumnRef col = colAcc(cb);
    if(i+col->Size() >= start) {  // only if this block was not fetched yet
      auto ccol = col->As<CT>();

      // first index within this block (note: can't use std::max(start-i, 0)
      // here, since the variables are unsigned, and i>start is possible)
      size_t localStart = (i < start ? start-i : 0),
      // one past the last index within this block
      // guaranteed to be >=0, since the loop is aborted if i >= start+len
             localEnd = std::min(start+len-i, col->Size());

      convFunc(cb, ccol, v, offset, localStart, localEnd);
      offset += localEnd-localStart;
    }

    i += col->Size();
    if(i >= start+len) {  // processed all blocks in the requested range
      break;
    }
  }
  df.push_back(v);
}

// helper function for converting a sequence of scalar column entries (can't be
// a member of Result due to C++ prohibiting explicit specialization on members
// of a non-specialized class)
template<typename CT, typename RT>
void convertEntries(std::shared_ptr<const CT> in, NullCol nullCol, RT &out,
    size_t offset, size_t start, size_t end) {
  for(size_t j = start; j < end; j++) {
    // can't use the ternary operator here, since that would require explicit
    // conversion from the Clickhouse storage type (which is far messier)
    if(nullCol && nullCol->IsNull(j)) {
      out[offset+j-start] = RT::get_na();
    } else {
      out[offset+j-start] = in->At(j);
    }
  }
}

template<>
void convertEntries<ch::ColumnDecimal, Rcpp::StringVector>(std::shared_ptr<const ch::ColumnDecimal> in, NullCol nullCol, Rcpp::StringVector &out,
                                                         size_t offset, size_t start, size_t end) {
  for(size_t j = start; j < end; j++) {
    // can't use the ternary operator here, since that would require explicit
    // conversion from the Clickhouse storage type (which is far messier)
    if(nullCol && nullCol->IsNull(j)) {
      out[offset+j-start] = Rcpp::StringVector::get_na();
    } else {
      out[offset+j-start] = (in->At(j).to_string());
    }
  }
}

template<>
void convertEntries<ch::ColumnInt64, Rcpp::StringVector>(std::shared_ptr<const ch::ColumnInt64> in, NullCol nullCol, Rcpp::StringVector &out,
                    size_t offset, size_t start, size_t end) {
  for(size_t j = start; j < end; j++) {
    // can't use the ternary operator here, since that would require explicit
    // conversion from the Clickhouse storage type (which is far messier)
    if(nullCol && nullCol->IsNull(j)) {
      out[offset+j-start] = Rcpp::StringVector::get_na();
    } else {
      out[offset+j-start] = std::to_string(in->At(j));
    }
  }
}


template<>
void convertEntries<ch::ColumnUInt64, Rcpp::StringVector>(std::shared_ptr<const ch::ColumnUInt64> in, NullCol nullCol, Rcpp::StringVector &out,
                                                         size_t offset, size_t start, size_t end) {
  for(size_t j = start; j < end; j++) {
    // can't use the ternary operator here, since that would require explicit
    // conversion from the Clickhouse storage type (which is far messier)
    if(nullCol && nullCol->IsNull(j)) {
      out[offset+j-start] = Rcpp::StringVector::get_na();
    } else {
      out[offset+j-start] = std::to_string(in->At(j));
    }
  }
}
// Date requires specialization: otherwise causes problems due to type
// ambiguities in the Rcpp::Date constructor, which expects either int or
// double, whereas ColumnDate values are uint32_t
template<>
void convertEntries<ch::ColumnDate, Rcpp::DateVector>(std::shared_ptr<const ch::ColumnDate> in,
    NullCol nullCol, Rcpp::DateVector &out, size_t offset, size_t start, size_t end) {
  for(size_t j = start; j < end; j++) {
    if(nullCol && nullCol->IsNull(j)) {
      out[offset+j-start] = Rcpp::DateVector::get_na();
    } else {
      out[offset+j-start] = static_cast<int>(in->At(j)/(60*60*24));
    }
  }
}

std::string formatUUID(const ch::UInt128 &v) {
  const size_t bufsize = 128/4 + 4 + 1;  // 128 bit in hexadecimal + 4 dashes + null terminator
  char buf[bufsize];

  std::snprintf(&buf[0], bufsize, "%08llx-%04llx-%04llx-%04llx-%012llx",
      static_cast<unsigned long long>(v.first>>32),
      (v.first>>16)&0xFFFFllu,
      v.first&0xFFFFllu,
      static_cast<unsigned long long>(v.second>>48),
      v.second&0xFFFFFFFFFFFFllu);
  return std::string(buf);
}

template<>
void convertEntries<ch::ColumnUUID, Rcpp::StringVector>(std::shared_ptr<const ch::ColumnUUID> in,
    NullCol nullCol, Rcpp::StringVector &out, size_t offset, size_t start, size_t end) {
  for(size_t j = start; j < end; j++) {
    if(nullCol && nullCol->IsNull(j)) {
      out[offset+j-start] = Rcpp::StringVector::get_na();
    } else {
      out[offset+j-start] = formatUUID(in->At(j));
    }
  }
}

template<typename VT>
using LevelMapT = std::map<VT, unsigned>;

template<typename CT, typename VT, typename RT>
void convertEnumEntries(std::shared_ptr<const CT> in, LevelMapT<VT> &levelMap,
    NullCol nullCol, RT &out, size_t offset, size_t start, size_t end) {
  for(size_t j = start; j < end; j++) {
    if(nullCol && nullCol->IsNull(j)) {
      out[offset+j-start] = RT::get_na();
    } else {
      out[offset+j-start] = levelMap[in->At(j)];
    }
  }
}

template<typename CT, typename RT>
class ScalarConverter : public Converter {
  void processBlocks(Result &r, Result::AccFunc colAcc, Rcpp::List &target,
      size_t start, size_t len, Result::AccFunc nullAcc) {
    r.convertTypedColumn<CT, RT>(colAcc, target, start, len,
        [&nullAcc](const Result::ColBlock &cb, std::shared_ptr<const CT> in,
          RT &out, size_t offset, size_t start, size_t end) {
      NullCol nullCol =
        nullAcc ? nullAcc(cb)->As<ch::ColumnNullable>() : nullptr;
      convertEntries<CT,RT>(in, nullCol, out, offset, start, end);
    });
  }

  void processCol(ch::ColumnRef col, Rcpp::List &target, size_t targetIdx,
      NullCol nullCol) {
    auto typedCol = col->As<CT>();
    RT v(col->Size());
    convertEntries<CT,RT>(typedCol, nullCol, v, 0, 0, col->Size());
    target[targetIdx] = v;
  }
};

class NullableConverter : public Converter {
  using CT = ch::ColumnNullable;
  std::unique_ptr<Converter> elemConverter;

public:
  NullableConverter(std::unique_ptr<Converter> elemProc) : elemConverter(std::move(elemProc)) {}

  //NOTE: nested nullable is not currently permitted in Clickhouse
  void processBlocks(Result &r, Result::AccFunc colAcc, Rcpp::List &target, size_t start, size_t len, Result::AccFunc) {
    elemConverter->processBlocks(r, [&colAcc](const Result::ColBlock &cb){return colAcc(cb)->As<CT>()->Nested();}, target, start, len, [&colAcc](const Result::ColBlock &cb){return colAcc(cb)->As<ch::ColumnNullable>();});
  }

  void processCol(ch::ColumnRef col, Rcpp::List &target, size_t targetIdx, NullCol) {
    auto typedCol = col->As<CT>();
    elemConverter->processCol(typedCol->Nested(), target, targetIdx, typedCol);
  }
};

class ArrayConverter : public Converter {
  std::unique_ptr<Converter> elemConverter;

public:
  ArrayConverter(std::unique_ptr<Converter> elemProc) : elemConverter(std::move(elemProc)) {}

  //NOTE: arrays can't be nested in a Nullable, so that can be ignored
  void processBlocks(Result &r, Result::AccFunc colAcc, Rcpp::List &target, size_t start, size_t len, Result::AccFunc) {
    using CT = ch::ColumnArray;
    using RT = Rcpp::List;
    auto eproc = std::move(elemConverter);
    r.convertTypedColumn<ch::ColumnArray, Rcpp::List>(colAcc, target, start, len, [&eproc](const Result::ColBlock &, std::shared_ptr<const CT> in, RT &out, size_t offset, size_t start, size_t end) {
      for(size_t j = start; j < end; j++) {
        ch::ColumnRef entry = in->GetAsColumn(j);
        eproc->processCol(entry, out, offset+j-start, nullptr);
      }
    });
  }

  void processCol(ch::ColumnRef, Rcpp::List &, size_t, NullCol) {
    throw std::invalid_argument("nested arrays are currently not supported");
  }
};

template<typename CT, typename VT, typename RT>
class EnumConverter : public Converter {
  ch::EnumType type;
  Rcpp::CharacterVector levels;
  LevelMapT<VT> levelMap;   // mapping from enum values in the column type to
                            // level indices in the R factor to be created

  void genLevelMap(LevelMapT<VT> &levelMap, Rcpp::CharacterVector &levels) {
    for (auto it = type.BeginValueToName(); it != type.EndValueToName(); it++) {
      levels.push_back(it->second);
      levelMap[it->first] = levels.size();  // note: R factor level indices start at 1
    }
  }

public:
  EnumConverter(ch::TypeRef type, const std::vector<EnumItem>& items) : type(type->GetCode(), items) {
    genLevelMap(levelMap, levels);
  }

  void processBlocks(Result &r, Result::AccFunc colAcc, Rcpp::List &target,
      size_t start, size_t len, Result::AccFunc nullAcc) {

    r.convertTypedColumn<CT, RT>(colAcc, target, start, len,
        [&](const Result::ColBlock &cb, std::shared_ptr<const CT> in,
          RT &out, size_t offset, size_t start, size_t end) {
      NullCol nullCol =
        nullAcc ? nullAcc(cb)->As<ch::ColumnNullable>() : nullptr;
      convertEnumEntries<CT,VT,RT>(in, levelMap, nullCol, out, offset, start, end);

      out.attr("class") = "factor";
      out.attr("levels") = levels;
    });
  }
  void processCol(ch::ColumnRef col, Rcpp::List &target, size_t targetIdx,
      NullCol nullCol) {
    auto typedCol = col->As<CT>();
    RT v(col->Size());

    convertEnumEntries<CT,VT,RT>(typedCol, levelMap, nullCol, v, 0, 0, col->Size());

    v.attr("class") = "factor";
    v.attr("levels") = levels;
    target[targetIdx] = v;
  }
};

std::unique_ptr<Converter> Result::buildConverter(std::string name, ch::TypeRef type) const {
  using TC = ch::Type::Code;

  switch(type->GetCode()) {
    case TC::Int8:
      return std::unique_ptr<ScalarConverter<ch::ColumnInt8, Rcpp::IntegerVector>>(new ScalarConverter<ch::ColumnInt8, Rcpp::IntegerVector>);
    case TC::Int16:
      return std::unique_ptr<ScalarConverter<ch::ColumnInt16, Rcpp::IntegerVector>>(new ScalarConverter<ch::ColumnInt16, Rcpp::IntegerVector>);
    case TC::Int32:
      return std::unique_ptr<ScalarConverter<ch::ColumnInt32, Rcpp::IntegerVector>>(new ScalarConverter<ch::ColumnInt32, Rcpp::IntegerVector>);
    case TC::Int64:
      return std::unique_ptr<ScalarConverter<ch::ColumnInt64, Rcpp::StringVector>>(new ScalarConverter<ch::ColumnInt64, Rcpp::StringVector>);
    case TC::UInt8:
      return std::unique_ptr<ScalarConverter<ch::ColumnUInt8, Rcpp::IntegerVector>>(new ScalarConverter<ch::ColumnUInt8, Rcpp::IntegerVector>);
    case TC::UInt16:
      return std::unique_ptr<ScalarConverter<ch::ColumnUInt16, Rcpp::IntegerVector>>(new ScalarConverter<ch::ColumnUInt16, Rcpp::IntegerVector>);
    case TC::UInt32: {
      warn("column "+name+" converted from UInt32 to Numeric");
      return std::unique_ptr<ScalarConverter<ch::ColumnUInt32, Rcpp::NumericVector>>(new ScalarConverter<ch::ColumnUInt32, Rcpp::NumericVector>);
    }
    case TC::UInt64: {
      return std::unique_ptr<ScalarConverter<ch::ColumnUInt64, Rcpp::StringVector>>(new ScalarConverter<ch::ColumnUInt64, Rcpp::StringVector>);
    }
    case TC::Decimal: {
      return std::unique_ptr<ScalarConverter<ch::ColumnDecimal, Rcpp::StringVector>>(new ScalarConverter<ch::ColumnDecimal, Rcpp::StringVector>);
    }
    case TC::UUID:
      return std::unique_ptr<ScalarConverter<ch::ColumnUUID, Rcpp::StringVector>>(new ScalarConverter<ch::ColumnUUID, Rcpp::StringVector>);
    case TC::Float32:
      return std::unique_ptr<ScalarConverter<ch::ColumnFloat32, Rcpp::NumericVector>>(new ScalarConverter<ch::ColumnFloat32, Rcpp::NumericVector>);
    case TC::Float64:
      return std::unique_ptr<ScalarConverter<ch::ColumnFloat64, Rcpp::NumericVector>>(new ScalarConverter<ch::ColumnFloat64, Rcpp::NumericVector>);
    case TC::String:
      return std::unique_ptr<ScalarConverter<ch::ColumnString, Rcpp::StringVector>>(new ScalarConverter<ch::ColumnString, Rcpp::StringVector>);
    case TC::FixedString:
      return std::unique_ptr<ScalarConverter<ch::ColumnFixedString, Rcpp::StringVector>>(new ScalarConverter<ch::ColumnFixedString, Rcpp::StringVector>);
    case TC::DateTime:
      return std::unique_ptr<ScalarConverter<ch::ColumnDateTime, Rcpp::DatetimeVector>>(new ScalarConverter<ch::ColumnDateTime, Rcpp::DatetimeVector>);
    case TC::Date:
      return std::unique_ptr<ScalarConverter<ch::ColumnDate, Rcpp::DateVector>>(new ScalarConverter<ch::ColumnDate, Rcpp::DateVector>);
    case TC::Nullable:
      {
        // downcast to NullableType to access GetNestedType member
        std::shared_ptr<class ch::NullableType> nullable_t = std::static_pointer_cast<ch::NullableType>(type);

        return std::unique_ptr<NullableConverter>(new NullableConverter(buildConverter(name, nullable_t->GetNestedType())));
      }
    case TC::Array:
      {
        // downcast to ArrayType to access GetItemType member
        std::shared_ptr<class ch::ArrayType> array_t = std::static_pointer_cast<ch::ArrayType>(type);

        return std::unique_ptr<ArrayConverter>(new ArrayConverter(buildConverter(name, array_t->GetItemType())));
      }
    case TC::Enum8:
      {
        // downcast to EnumType to access items member
        auto enum_t = std::static_pointer_cast<ch::EnumType>(type);
        // extract items
        std::vector<EnumItem> items;

        for (auto iter = enum_t->BeginValueToName(); iter != enum_t->EndValueToName(); iter++ ){
          items.push_back(EnumItem(iter->second, iter->first));
        }

        return std::unique_ptr<EnumConverter<ch::ColumnEnum8, int8_t, Rcpp::IntegerVector>>(new EnumConverter<ch::ColumnEnum8, int8_t, Rcpp::IntegerVector>(type, items));
      }
    case TC::Enum16:
      {
        // downcast to EnumType to access items member
        auto enum_t = std::static_pointer_cast<ch::EnumType>(type);
        // extract items
        std::vector<EnumItem> items;

        for (auto iter = enum_t->BeginValueToName(); iter != enum_t->EndValueToName(); iter++ ){
          items.push_back(EnumItem(iter->second, iter->first));
        }

        return std::unique_ptr<EnumConverter<ch::ColumnEnum16, int16_t, Rcpp::IntegerVector>>(new EnumConverter<ch::ColumnEnum16, int16_t, Rcpp::IntegerVector>(type, items));
      }
    default:
      throw std::invalid_argument("cannot read unsupported type: "+type->GetName());
      break;
  }
}

void Result::setColInfo(const ch::Block &block) {
  for(ch::Block::Iterator bi(block); bi.IsValid(); bi.Next()) {
    colNames.push_back(Rcpp::String(bi.Name()));
    colTypes.push_back(bi.Type());
    colTypesString.push_back(bi.Type()->GetName());
  }
}

bool Result::isComplete() const {
  return fetchedRows >= availRows;
}

size_t Result::numFetchedRows() const {
  return fetchedRows;
}

size_t Result::numRowsAffected() const {
  return 0;   //TODO
}

std::string Result::getStatement() const {
  return statement;
}

Result::TypeList Result::getColTypes() const {
  return colTypes;
}

void Result::addBlock(const ch::Block &block) {
  if(static_cast<size_t>(colNames.size()) < block.GetColumnCount()) {
    setColInfo(block);
  }

  if(block.GetRowCount() > 0) {   // don't add empty blocks
    ColBlock cb;
    for(ch::Block::Iterator bi(block); bi.IsValid(); bi.Next()) {
      cb.columns.push_back(bi.Column());
    }
    columnBlocks.push_back(cb);
    availRows += block.GetRowCount();
  }
}

Rcpp::DataFrame Result::fetchFrame(ssize_t n) {
  size_t nRows = n >= 0 ? std::min(static_cast<size_t>(n), availRows-fetchedRows) : availRows-fetchedRows;
  Rcpp::DataFrame df;

  for(size_t i = 0; i < static_cast<size_t>(colNames.size()); i++) {
    //TODO: it would be sufficient to build the Converter just once
    std::unique_ptr<Converter> proc = buildConverter(std::string(colNames[i]), colTypes[i]);
    proc->processBlocks(*this, [&i](const ColBlock &cb){return cb.columns[i];}, df, fetchedRows, nRows, nullptr);
    //TODO: release blocks once they have been fetched
  }

  df.attr("class") = "data.frame";
  if(nRows > 0) {
    df.attr("row.names") = Rcpp::Range(fetchedRows+1, fetchedRows+nRows);
  }
  df.attr("names") = colNames;
  df.attr("data.type") = colTypesString;
  fetchedRows += nRows;

  return df;
}
