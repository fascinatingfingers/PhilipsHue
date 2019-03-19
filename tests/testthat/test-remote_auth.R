
context('remote_auth_request_url')

test_that('helper creates URL to request access to bridge from client', {
    mockery::stub(remote_auth_request_url, 'uuid::UUIDgenerate', 'mock_uuid')
    expect_identical(
        remote_auth_request_url('client_id', 'app id', 'device_id', 'device name'),
        'https://api.meethue.com/oauth2/auth?clientid=client_id&appid=app%20id&deviceid=device_id&devicename=device%20name&state=mock_uuid&response_type=code'
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
