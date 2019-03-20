
context('remote_auth_request_url')

test_that('helper creates URL to request access to bridge from client', {
    mockery::stub(remote_auth_request_url, 'digest::digest', 'mock_hash')
    expect_identical(
        remote_auth_request_url('client_id', 'app id', 'device_id', 'device name'),
        'https://api.meethue.com/oauth2/auth?clientid=client_id&appid=app%20id&deviceid=device_id&devicename=device%20name&state=mock_hash&response_type=code'
    )
})



context('token')

test_that('`token` throws error and prints parsed content if POST fails', {
    mockery::stub(token, 'httr::POST', list())
    mockery::stub(token, 'httr::status_code', 'mock status code')
    mockery::stub(token, 'httr::content', list(errors = c('mock error 1')))

    expect_error(token('mock auth code'), 'mock status code')
    expect_error(token('mock auth code'), 'mock error 1')
})

test_that('`token` returns token if POST succeeds', {
    mockery::stub(token, 'httr::POST', list())
    mockery::stub(token, 'httr::status_code', 200)
    mockery::stub(token, 'httr::content', list(access_token = 'mock access token'))

    expect_identical(token('mock auth code'), 'mock access token')
})



context('remote_button_press')

test_that('`remote_button_press` throws error and prints parsed content if POST fails', {
    mockery::stub(remote_button_press, 'httr::PUT', list())
    mockery::stub(remote_button_press, 'httr::status_code', 'mock status code')
    mockery::stub(remote_button_press, 'httr::content', list(errors = c('mock error 1')))

    expect_error(remote_button_press('mock access token'), 'mock status code')
    expect_error(remote_button_press('mock access token'), 'mock error 1')
})

test_that('`remote_button_press` returns TRUE if POST succeeds', {
    mockery::stub(remote_button_press, 'httr::POST', list())
    mockery::stub(remote_button_press, 'httr::status_code', 200)

    expect_true(remote_button_press('mock access token'))
})



context('app_username')

test_that('`app_username` throws error and prints parsed content if POST fails', {
    mockery::stub(app_username, 'httr::POST', list())
    mockery::stub(app_username, 'httr::status_code', 'mock status code')
    mockery::stub(app_username, 'httr::content', list(errors = c('mock error 1')))

    expect_error(app_username('mock access token', 'mock_app_id'), 'mock status code')
    expect_error(app_username('mock access token', 'mock_app_id'), 'mock error 1')
})

test_that('`app_username` returns username if POST succeeds', {
    mockery::stub(app_username, 'httr::POST', list())
    mockery::stub(app_username, 'httr::status_code', 200)
    mockery::stub(app_username, 'httr::content', list(list(success = list(username = 'mock username'))))

    expect_identical(app_username('mock access token', 'mock_app_id'), 'mock username')
})
