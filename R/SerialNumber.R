
#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' SerialNumberSelectServer()
SerialNumberSelectServer <- function(input,output,session,dms_token,erp_token) {
  #获取参数
  text_SerialNumber=tsui::var_text('text_SerialNumber')


  #查询按钮

  shiny::observeEvent(input$btn_SerialNumber_view,{

    FSerialNumber=text_SerialNumber()


    if(FSerialNumber==''){

      tsui::pop_notice("Please Enter Serial Number")


    }else{
      data = mdlVmSerialNumberPkg::SerialNumber_select(erp_token = erp_token,FSerialNumber =FSerialNumber )
      tsui::run_dataTable2(id ='SerialNumber_resultView' ,data =data )

      tsui::run_download_xlsx(id = 'dl_SerialNumber',data = data,filename = 'SerialNumber.xlsx')


    }


  })



}


#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' SerialNumberServer()
SerialNumberServer <- function(input,output,session,dms_token,erp_token) {
  SerialNumberSelectServer(input = input,output = output,session = session,dms_token = dms_token,erp_token=erp_token)


}


