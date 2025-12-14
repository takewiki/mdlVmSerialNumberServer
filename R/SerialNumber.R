
#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param app_id
#' @param run_env
#' @param session 会话
#'
#' @return 返回值
#' @export
#'
#' @examples
#' SerialNumberSelectServer()
SerialNumberSelectServer <- function(input,output,session, app_id, run_env = "PRD") {
  # 获取所有字段名
  SerialNumber_all_columns <- c(
      'Serial Number',
      'Production Date',
      'Sales OrderID',
      'Delivery Location',
      'Sales OrderID2',
      'PN',
      'Product Name',
      'Sales OrderQty',
      'Delivery Date',
      'Total DeliveryQty'
    )

  #设置默认值
  SerialNumber_default_columns <- c(
      'Serial Number',
      'PN',
      'Product Name',
      'Sales OrderID',
      'Delivery Location',
      'Production Date'
    )
  SerialNumber_reset_columns <- c(
      'Serial Number',
      'PN',
      'Product Name'
    )

  # 全选按钮
  observeEvent(input$btn_SerialNumber_select_all, {
    updatePickerInput(
      session = session,
      inputId = "SerialNumber_column_selector",
      selected = SerialNumber_all_columns
    )
  })

  # 取消全选按钮
  observeEvent(input$btn_SerialNumber_deselect_all, {
    updatePickerInput(
      session = session,
      inputId = "SerialNumber_column_selector",
      selected = SerialNumber_reset_columns
    )
  })
  # 默认值按钮
  observeEvent(input$btn_SerialNumber_defaultValue, {
    updatePickerInput(
      session = session,
      inputId = "SerialNumber_column_selector",
      selected = SerialNumber_default_columns
    )
  })

  #获取参数
  text_SerialNumber=tsui::var_text('text_SerialNumber')

  # 创建反应式数据
  # SerialNumber_data <- reactiveVal({
  #   FSerialNumber=text_SerialNumber()
  #   if(FSerialNumber==''){
  #     tsui::pop_notice("Please Enter Serial Number")
  #   }else{
  #     erp_token = rdbepkg::dbConfig(FAppId = app_id, FType = "ERP", FRunEnv = run_env)
  #     data = mdlVmSerialNumberPkg::SerialNumber_select(erp_token = erp_token,FSerialNumber =FSerialNumber )
  #   }
  #
  # })
  #处理过程后的字段
  # SerialNumber_filtered_data <- reactive({
  #   req(input$SerialNumber_column_selector)
  #
  #   SerialNumber_df <- SerialNumber_data()
  #
  #   # 确保至少选择一个字段
  #   if (length(input$SerialNumber_column_selector) == 0) {
  #     return(data.frame(Message = "Please Choose One Column at least!"))
  #   }
  #    # 选择指定的列
  #   SerialNumber_selected_df <- SerialNumber_df[, input$SerialNumber_column_selector, drop = FALSE]
  # })


  #查询按钮
  # 显示选择信息
  output$SerialNumberselection_info <- renderPrint({
    selected <- input$SerialNumber_column_selector
    cat("Column Count: ", length(selected), "\n")
    cat("Column List: ", paste(selected, collapse = ", "), "\n")
    FSerialNumber=text_SerialNumber()
    cat("Serial Number:",FSerialNumber)
  })

  shiny::observeEvent(input$btn_SerialNumber_view,{

    #req(SerialNumber_filtered_data())
    #req(input$SerialNumber_column_selector)
    FSerialNumber=text_SerialNumber()


    if(FSerialNumber==''){

      tsui::pop_notice("Please Enter Serial Number")


    }else{

      erp_token = rdbepkg::dbConfig(FAppId = app_id, FType = "ERP", FRunEnv = run_env)
      data = mdlVmSerialNumberPkg::SerialNumber_select(erp_token = erp_token,FSerialNumber =FSerialNumber )
      print(data)
      data_selected = data[ ,input$SerialNumber_column_selector,drop=FALSE]


      tsui::run_dataTable2(id ='SerialNumber_resultView' ,data =data_selected,lang = 'en' )

      tsui::run_download_xlsx(id = 'dl_SerialNumber',data = data_selected,filename = 'SerialNumber.xlsx')


    }


  })



}


#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param app_id
#' @param run_env
#' @param session 会话
#'
#' @return 返回值
#' @export
#'
#' @examples
#' SerialNumberServer()
SerialNumberServer <- function(input,output,session, app_id, run_env = "PRD") {
  SerialNumberSelectServer(input = input,output = output,session = session,app_id=app_id, run_env = run_env)


}


