*"* components of interface IF_CWD_CONSTANTS
interface IF_CWD_CONSTANTS
  public .

  type-pools COBAI .

  constants C_YES type BOOLE_D value 'X'. "#EC NOTEXT
  constants C_NO type BOOLE_D value ' '. "#EC NOTEXT
  constants C_ON type BOOLE_D value 'X'. "#EC NOTEXT
  constants C_OFF type BOOLE_D value ' '. "#EC NOTEXT
  constants C_LOG_OBJ type BALOBJ_D value 'CAMPAIGN_DISPENSING'. "#EC NOTEXT
  constants C_BATCH_MODE type BALMODE value 'B'. "#EC NOTEXT
  constants C_MESSAGE_CLASS_VERY_IMPORTANT type BALPROBCL value '1'. "#EC NOTEXT
  constants C_MESSAGE_CLASS_IMPORTANT type BALPROBCL value '2'. "#EC NOTEXT
  constants C_MESSAGE_CLASS_MEDIUM type BALPROBCL value '3'. "#EC NOTEXT
  constants C_MESSAGE_CLASS_INFORMATION type BALPROBCL value '4'. "#EC NOTEXT
  constants C_MESSAGE_CLASS_OTHER type BALPROBCL value ''. "#EC NOTEXT
  constants C_CWD_MESSAGE_CLASS type SYMSGID value 'CWD'. "#EC NOTEXT
  constants C_MESSAGE_ERROR type SYMSGTY value 'E'. "#EC NOTEXT
  constants C_MESSAGE_SUCCESS type SYMSGTY value 'S'. "#EC NOTEXT
  constants C_MESSAGE_INFO type SYMSGTY value 'I'. "#EC NOTEXT
  constants C_MESSAGE_WARNING type SYMSGTY value 'W'. "#EC NOTEXT
  constants C_MESSAGE_ABORT type SYMSGTY value 'A'. "#EC NOTEXT
  constants C_SIGN_INCLUSIVE type TVARV_SIGN value 'I'. "#EC NOTEXT
  constants C_SIGN_EXCLUSIVE type TVARV_SIGN value 'E'. "#EC NOTEXT
  constants C_OPTION_BETWEEN type TVARV_OPTI value 'BT'. "#EC NOTEXT
  constants C_OPTION_EQUALS type TVARV_OPTI value 'EQ'. "#EC NOTEXT
  constants C_OPTION_LESS_THAN type TVARV_OPTI value 'LT'. "#EC NOTEXT
  constants C_OPTION_LESS_THAN_EQUALS type TVARV_OPTI value 'LE'. "#EC NOTEXT
  constants C_OPTION_GREATER_THAN type TVARV_OPTI value 'GT'. "#EC NOTEXT
  constants C_OPTION_GREATER_THAN_EQUALS type TVARV_OPTI value 'GE'. "#EC NOTEXT
  constants C_CLASS_TYPE type BAPI_CLASS_KEY-CLASSTYPE value '002'. "#EC NOTEXT
  constants C_DEST_TYPE type BAPI_RCOCHAPI-DEST_TYPE value '3'. "#EC NOTEXT
  constants C_PROCESS_INSTR type CO_VORGA value 'AMAT_1'. "#EC NOTEXT
  constants C_MATERIAL_CATEGORY type ATNAM value 'PPPI_MATERIAL'. "#EC NOTEXT
  constants C_MATERIAL_TEXT_CATEGORY type ATNAM value 'PPPI_MATERIAL_SHORT_TEXT'. "#EC NOTEXT
  constants C_MATERIAL_QUANTITY type ATNAM value 'PPPI_MATERIAL_QUANTITY'. "#EC NOTEXT
  constants C_MATERIAL_BATCH type ATNAM value 'PPPI_BATCH'. "#EC NOTEXT
  constants C_UOM type ATNAM value 'PPPI_UNIT_OF_MEASURE'. "#EC NOTEXT
  constants C_CO_PRODUCT type ATNAM value 'PPPI_MATERIAL_CO_PRODUCT'. "#EC NOTEXT
  constants C_RESERVATION type ATNAM value 'PPPI_RESERVATION'. "#EC NOTEXT
  constants C_RESERVATION_ITEM type ATNAM value 'PPPI_RESERVATION_ITEM'. "#EC NOTEXT
  constants C_STORAGE_LOCATION type ATNAM value 'PPPI_STORAGE_LOCATION'. "#EC NOTEXT
  constants C_PHASE type ATNAM value 'PPPI_PHASE'. "#EC NOTEXT
endinterface.
