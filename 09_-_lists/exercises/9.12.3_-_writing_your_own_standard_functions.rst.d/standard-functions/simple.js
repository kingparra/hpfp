// function executeScript(user, someScript, inputParameterArray) {
// }
// let user = { name: "Chris", permissions: "777"}
function pingGoogle() {

    let url = "www.google.com"
    let addr = `https://steakovercooked.com/api/ping/?host={url}`
    $.get(url,successCallback)

}


function successCallback(data) {
    alert(data)
}
