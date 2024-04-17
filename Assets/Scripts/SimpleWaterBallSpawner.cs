using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SimpleWaterBallSpawner : MonoBehaviour//脚本用于第一关自动生成水精华
{
    public GameObject WaterBall;
    public float internalTime;
    public Vector2 SpawnRange;
    public Vector2 SizeRange;
    public int MaxCount = 10;
    private float time;
    private int count = 0;
    
    void Start()
    {
        
    }

    // Update is called once per frame
    void FixedUpdate()
    {
        time += Time.fixedDeltaTime;
        if (time >= internalTime&&count<=MaxCount)
        {
           SpawnWaterBall(SpawnRange);
           time = 0f;
           count++;
        }
    }

    void SpawnWaterBall(Vector2 spawnRange)
    {
        float rdX = Random.Range(-spawnRange.x, spawnRange.x);
        float rdY = Random.Range(-spawnRange.y, spawnRange.y);
        var spawnedWaterBall=Instantiate(WaterBall, this.transform.position + new Vector3(rdX, 0,rdY),this.transform.rotation);
        var size = Random.Range(SizeRange.x, SizeRange.y);
        spawnedWaterBall.transform.localScale = new Vector3(size, size, size);
    }
}
