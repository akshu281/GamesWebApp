$(document).ready(function(){
/* initial setup */
	$("#faceshape").hide();
	$("#output").hide();
	
	$("#addfact").submit(function(event){
		event.preventDefault();
		name=$('#personname').val();
		age=$("#age").val();
		occupation=$("#occupation").val();
		data_to_server={"data":"(person (name "+name+")(age "+age+")(occupation "+occupation+"))"};
		$.ajax({
			url:'ClipsServer/addfact',
			data:data_to_server,
			
			method:'post',
			success:(function(response){
				console.log(response.toString())
				
			})
		})
	})
	$("#getfacts").click(function(){
		$.ajax({
			url:'ClipsServer/getfacts/person',
			success:function(data){
				console.log(data)
			}
		})
	})
		
	
})