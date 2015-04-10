# Methods v 1.4
#################################################################################
                  #generic_methods_declaration_section_start
#################################################################################

#begin_RawDataRecord_generic_methods_declaration_block
setGeneric("mScanHeaderFor", function(self, forwhat, keystring){standardGeneric("mScanHeaderFor")})

setGeneric("mGetSamples", function(self, fields){standardGeneric("mGetSamples")})

setGeneric("mGetSubjectCode", function(self, settings){standardGeneric("mGetSubjectCode")})
setGeneric("mSetSubjectCode", function(self, settings){standardGeneric("mSetSubjectCode")})

setGeneric("mGetExpID", function(self, settings){standardGeneric("mGetExpID")})
setGeneric("mSetExpID", function(self, settings){standardGeneric("mSetExpID")})

setGeneric("mGetSettings", function(self, settings){standardGeneric("mGetSettings")})
setGeneric("mSetSettings", function(self, settings){standardGeneric("mSetSettings")})
#end_RawDataRecord_generic_methods_declaration_block


#begin_RawDataTable_generic_methods_declaration_block
setGeneric("mLoadRecords", function(self, path, parser){standardGeneric("mLoadRecords")})
setGeneric("mAddRecords", function(self, path, parser){standardGeneric("mAddRecords")})

setGeneric("mGetAllSamples", function(self, fields){standardGeneric("mGetAllSamples")})

setGeneric("mTrialsDataExtraction", function(self, parser){standardGeneric("mTrialsDataExtraction")})
#end_RawDataTable_generic_methods_declaration_block


#begin_ExpData_generic_methods_declaration_block
#setGeneric("mLoadData", function(self, records){standardGeneric("mAddRecords")})
#end_ExpData_generic_methods_declaration_block

#########################################################################################
                      #generic_methods_declaration_section_end
#########################################################################################

####################################################################################
                        #methods_realization_section_start
####################################################################################


#begin_RawDataTable_generic_methods_realization_block
setMethod("mLoadRecords", "RawDataTable",                                   
          function(self, path, parser)
          {
            if (parser@name != "Own")
            {
              source(parser@funpath)
            }
            fun <- parser@funname
            funSettings <- parser@funSettings
            rawDataRecords <- fun(path, funSettings)
            self <- rawDataRecords
            return(self)
          }
)

setMethod("mAddRecords", "RawDataTable",                                   
          function(self, path, parser)
          {
            if (parser@name != "Own")
            {
              source(parser@funpath)
            }
            fun <- parser@funname
            funSettings <- parser@funSettings
            rawDataRecords <- fun(path, funSettings)
            self <- append(self, rawDataRecords)
            return(self)
          }
)

#end_RawDataTable_generic_methods_realization_block

####################################################################################
                        #methods_realization_section_end
####################################################################################